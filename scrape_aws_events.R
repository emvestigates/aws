# R script: scrape dynamic table rows that open a popup/drawer, save details as TXT files
# Requirements:
# - R packages: RSelenium, stringr
# - A Selenium Chrome session running locally (easiest via Docker)
#
# 1) Start Selenium in a terminal first (recommended):
#    docker run -d --rm -p 4444:4444 -p 7900:7900 selenium/standalone-chrome:latest
#    - You can watch and interact with the browser at: http://localhost:7900/
#      (If asked for a password, try: secret)
#    - Log in to the target site there if required, then run this script.
#
# 2) Set the target URL below and run the script.

library(RSelenium)
library(stringr)

# -------------------- CONFIG --------------------
url <- "https://YOUR-TARGET-URL-HERE"   # TODO: replace with the page that contains the table shown in your example
out_dir <- "aws_event_txt"               # output directory for TXT files
headless <- TRUE                          # set to FALSE if you want to see the browser
max_rows <- Inf                           # limit rows to scrape if needed (e.g., 10)
# ------------------------------------------------

sanitize_filename <- function(x, max_len = 120) {
  x <- str_trim(x)
  x <- str_replace_all(x, "[^A-Za-z0-9._-]+", "_")
  substr(x, 1, max_len)
}

# Simple wait helper for XPath
wait_for_xpath <- function(driver, xpath, timeout = 15000, poll = 500) {
  t0 <- Sys.time()
  repeat {
    els <- tryCatch(driver$findElements(using = "xpath", value = xpath), error = function(e) list())
    if (length(els) > 0) return(els)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000 > timeout) {
      stop(paste("Timeout waiting for:", xpath), call. = FALSE)
    }
    Sys.sleep(poll / 1000)
  }
}

# Try to extract the text from the open popup/drawer/modal
extract_panel_text <- function(driver, timeout = 15000) {
  selectors <- c(
    "//div[@aria-modal='true']",                  # modal dialog
    "//div[@role='dialog']",                      # generic dialog
    "//div[contains(@class,'awsui_drawer')]",     # AWSUI drawer container
    "//div[contains(@class,'awsui-modal')]",      # AWSUI modal container
    "//aside[contains(@class,'drawer')]",         # generic drawer/aside
    "//div[contains(@class,'awsui')]//div[contains(@class,'content')]" # fallback
  )
  texts <- character(0)
  for (sel in selectors) {
    els <- tryCatch(wait_for_xpath(driver, sel, timeout = 2000), error = function(e) list())
    if (length(els)) {
      for (el in els) {
        t <- tryCatch(el$getElementText()[[1]], error = function(e) "")
        if (!is.null(t) && nzchar(t)) texts <- c(texts, t)
      }
    }
  }
  texts <- unique(texts)
  paste(texts, collapse = "\n\n---\n\n")
}

# Attempt to close the popup/drawer
close_panel_if_open <- function(driver) {
  # Common close controls (X buttons, Close text buttons, ESC)
  xpaths <- c(
    "//button[@aria-label='Close']",
    "//button[@title='Close']",
    "//div[contains(@class,'awsui')]//button[contains(@class,'close')]",
    "//span[text()='Close']/ancestor::button",
    "//button[.//span[contains(text(),'Close')]]"
  )
  for (xp in xpaths) {
    btns <- tryCatch(driver$findElements(using = 'xpath', value = xp), error = function(e) list())
    if (length(btns)) {
      try(btns[[1]]$click(), silent = TRUE)
      Sys.sleep(0.5)
      return(invisible(TRUE))
    }
  }
  # As a fallback, send ESC to body
  body <- tryCatch(driver$findElement('css selector', 'body'), error = function(e) NULL)
  if (!is.null(body)) {
    try(body$sendKeysToElement(list(key = 'escape')))
  }
  invisible(FALSE)
}

# Ensure output dir exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Connect to Selenium (assumes remote at localhost:4444)
remote_driver <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "chrome",
  extraCapabilities = if (headless) list("goog:chromeOptions" = list(args = list("--headless=new","--disable-gpu","--window-size=1280,1200"))) else list()
)

remote_driver$open()

on.exit({
  try(remote_driver$close(), silent = TRUE)
  try(remote_driver$quit(), silent = TRUE)
}, add = TRUE)

# Navigate
remote_driver$navigate(url)

# Wait for table rows
rows_xpath <- "//table[contains(@class,'awsui_table')]//tbody//tr"
rows <- wait_for_xpath(remote_driver, rows_xpath, timeout = 20000)

n <- length(rows)
if (!is.infinite(max_rows)) n <- min(n, max_rows)

cat(sprintf("Found %d rows. Beginning scrape...\n", n))

# Iterate rows by re-querying each time to avoid stale references
for (i in seq_len(n)) {
  Sys.sleep(0.3)  # brief settle time
  rows <- tryCatch(remote_driver$findElements(using = "xpath", value = rows_xpath), error = function(e) list())
  if (length(rows) < i) break

  # Click the link in first data column (2nd td): an <a> with role='button'
  link_xpath <- sprintf("(%s)[%d]//td[2]//a", rows_xpath, i)
  link_el <- tryCatch(remote_driver$findElement(using = "xpath", value = link_xpath), error = function(e) NULL)
  if (is.null(link_el)) {
    cat(sprintf("Row %d: link not found, skipping.\n", i))
    next
  }

  # Grab event title for filename
  title_text <- tryCatch(link_el$getElementText()[[1]], error = function(e) sprintf("row_%03d", i))
  safe_name <- sanitize_filename(title_text)
  out_file <- file.path(out_dir, paste0(safe_name, ".txt"))

  # Click to open panel
  try(link_el$click(), silent = TRUE)

  # Small wait for panel to appear
  Sys.sleep(1.0)

  # Extract panel text
  panel_text <- tryCatch(extract_panel_text(remote_driver, timeout = 8000), error = function(e) "")

  if (!nzchar(panel_text)) {
    # Try clicking again if first attempt fails
    Sys.sleep(0.5)
    try(link_el$click(), silent = TRUE)
    Sys.sleep(1.0)
    panel_text <- tryCatch(extract_panel_text(remote_driver, timeout = 8000), error = function(e) "")
  }

  if (nzchar(panel_text)) {
    # Also capture the summary cells from the row for context
    row_text <- tryCatch({
      row_xpath <- sprintf("(%s)[%d]", rows_xpath, i)
      row_el <- remote_driver$findElement(using = "xpath", value = row_xpath)
      row_el$getElementText()[[1]]
    }, error = function(e) "")

    full_text <- paste0(
      "Title: ", title_text, "\n\n",
      "Row summary:\n",
      row_text, "\n\n",
      "Details (panel):\n",
      panel_text, "\n"
    )

    writeLines(full_text, con = out_file, useBytes = TRUE)
    cat(sprintf("Saved: %s\n", out_file))
  } else {
    cat(sprintf("Row %d: no panel text captured.\n", i))
  }

  # Close panel (best effort)
  close_panel_if_open(remote_driver)
  Sys.sleep(0.5)
}

cat("Done.\n")
