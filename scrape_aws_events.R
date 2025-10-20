# R script to run in GitHub Actions (or locally) to scrape AWSUI table popups and save as TXT files
#
# Configuration via environment variables:
#   TARGET_URL   (required)  - URL of the page containing the table
#   OUT_DIR      (optional)  - output directory for TXT files (default: aws_event_txt)
#   MAX_ROWS     (optional)  - maximum number of rows to scrape (default: all)
#   SELENIUM_HOST(optional)  - Selenium host (default: localhost)
#   SELENIUM_PORT(optional)  - Selenium port (default: 4444)
#   TAB_TEXT     (optional)  - Tab label to click to expose the table (default: "Service history")
#   ROWS_XPATH   (optional)  - Custom XPath to find table rows (overrides defaults)
#
# This script assumes a Selenium Standalone Chrome container is running and reachable.

library(RSelenium)
library(stringr)

# -------------------- Read ENV --------------------
url <- Sys.getenv("TARGET_URL", unset = "")
if (!nzchar(url)) stop("TARGET_URL env var is required.")

out_dir <- Sys.getenv("OUT_DIR", unset = "aws_event_txt")
max_rows_env <- Sys.getenv("MAX_ROWS", unset = "")
max_rows <- if (nzchar(max_rows_env)) suppressWarnings(as.integer(max_rows_env)) else Inf
if (!is.finite(max_rows)) max_rows <- Inf

selenium_host <- Sys.getenv("SELENIUM_HOST", unset = "localhost")
selenium_port <- suppressWarnings(as.integer(Sys.getenv("SELENIUM_PORT", unset = "4444")))
if (is.na(selenium_port)) selenium_port <- 4444L

tab_text <- Sys.getenv("TAB_TEXT", unset = "Service history")
rows_xpath_override <- Sys.getenv("ROWS_XPATH", unset = "")
# --------------------------------------------------

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
  # Fallback: send ESC to body
  body <- tryCatch(driver$findElement('css selector', 'body'), error = function(e) NULL)
  if (!is.null(body)) {
    try(body$sendKeysToElement(list(key = 'escape')))
  }
  invisible(FALSE)
}

# Accept cookie/consent banners if present
accept_cookies <- function(driver) {
  xpaths <- c(
    "//button[normalize-space()='Accept']",
    "//button[normalize-space()='Accept all cookies']",
    "//button[normalize-space()='Allow all cookies']",
    "//button[normalize-space()='I agree']",
    "//button[.//span[normalize-space()='Accept']]"
  )
  for (xp in xpaths) {
    btns <- tryCatch(driver$findElements(using = 'xpath', value = xp), error = function(e) list())
    if (length(btns)) {
      try(btns[[1]]$click(), silent = TRUE)
      Sys.sleep(0.5)
      break
    }
  }
}

# Click a tab by visible text (e.g., "Service history")
click_tab_by_text <- function(driver, tab_text, timeout = 15000) {
  if (!nzchar(tab_text)) return(FALSE)
  xpaths <- c(
    sprintf("//*[@role='tab' and normalize-space()='%s']", tab_text),
    sprintf("//button[.//span[normalize-space()='%s'] or normalize-space()='%s']", tab_text, tab_text),
    sprintf("//a[normalize-space()='%s']", tab_text)
  )
  for (xp in xpaths) {
    els <- tryCatch(driver$findElements(using = 'xpath', value = xp), error = function(e) list())
    if (length(els)) {
      el <- els[[1]]
      sel <- tryCatch(el$getElementAttribute("aria-selected")[[1]], error = function(e) NA_character_)
      if (!is.na(sel) && identical(sel, "true")) return(TRUE)
      ok <- tryCatch({ el$click(); TRUE }, error = function(e) FALSE)
      Sys.sleep(0.8)
      if (!ok) {
        js <- "arguments[0].scrollIntoView({block:'center', inline:'center'}); arguments[0].click();"
        try(driver$executeScript(js, args = list(el)), silent = TRUE)
        Sys.sleep(0.8)
      }
      return(TRUE)
    }
  }
  FALSE
}

# Switch into any iframe that contains the target rows; return TRUE if switched
switch_to_table_context <- function(driver, candidate_rows_xpaths) {
  # Try default context first
  for (xp in candidate_rows_xpaths) {
    els <- tryCatch(driver$findElements(using = 'xpath', value = xp), error = function(e) list())
    if (length(els)) return(TRUE)
  }
  # Try iframes
  frames <- tryCatch(driver$findElements(using = 'css selector', value = 'iframe'), error = function(e) list())
  if (!length(frames)) return(FALSE)
  for (fr in frames) {
    try(driver$switchToFrame(fr), silent = TRUE)
    for (xp in candidate_rows_xpaths) {
      els <- tryCatch(driver$findElements(using = 'xpath', value = xp), error = function(e) list())
      if (length(els)) return(TRUE)
    }
    # Not here; go back and continue
    try(driver$switchToFrame(NULL), silent = TRUE)
  }
  FALSE
}

# Ensure output dir exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Connect to Selenium (remote)
extra_caps <- list("goog:chromeOptions" = list(args = list("--headless=new", "--disable-gpu", "--window-size=1280,1200")))
remote_driver <- remoteDriver(
  remoteServerAddr = selenium_host,
  port = selenium_port,
  browserName = "chrome",
  extraCapabilities = extra_caps
)

# Open with retries (service may need a few seconds)
opened <- FALSE
for (attempt in 1:15) {
  res <- try(remote_driver$open(), silent = TRUE)
  if (!inherits(res, "try-error")) { opened <- TRUE; break }
  Sys.sleep(2)
}
if (!opened) stop("Failed to open Selenium session after retries.")

on.exit({
  try(remote_driver$close(), silent = TRUE)
  try(remote_driver$quit(), silent = TRUE)
}, add = TRUE)

# Navigate
remote_driver$navigate(url)

# Dismiss cookies and optionally click tab
accept_cookies(remote_driver)
invisible(try(click_tab_by_text(remote_driver, tab_text), silent = TRUE))

# Candidate row XPaths
candidate_rows <- if (nzchar(rows_xpath_override)) c(rows_xpath_override) else c(
  "//table[contains(@class,'awsui_table')]//tbody//tr",
  "//table[contains(@class,'awsui')]//tbody//tr",
  "//table[@role='table']//tbody//tr",
  "(//tbody)[1]//tr"
)

# Switch to frame that contains the table (if any)
if (!switch_to_table_context(remote_driver, candidate_rows)) {
  # Go back to default content in case
  try(remote_driver$switchToFrame(NULL), silent = TRUE)
}

# Final wait for rows (try each candidate in order)
rows <- NULL
last_err <- NULL
for (xp in candidate_rows) {
  rows <- tryCatch(wait_for_xpath(remote_driver, xp, timeout = 45000), error = function(e) { last_err <<- e; NULL })
  if (!is.null(rows) && length(rows)) { rows_xpath <- xp; break }
}

if (is.null(rows) || !length(rows)) {
  # Debug dump
  cur_url <- tryCatch(remote_driver$getCurrentUrl()[[1]], error = function(e) "<unknown>")
  title <- tryCatch(remote_driver$getTitle()[[1]], error = function(e) "<unknown>")
  html <- tryCatch(remote_driver$getPageSource()[[1]], error = function(e) "")
  dbg_file <- file.path(out_dir, "debug_page_source.html")
  writeLines(html, dbg_file, useBytes = TRUE)
  stop(sprintf("Could not locate table rows after trying multiple selectors. URL: %s | Title: %s | Dumped: %s", cur_url, title, dbg_file))
}

n <- length(rows)
if (!is.infinite(max_rows)) n <- min(n, max_rows)

cat(sprintf("Found %d rows. Beginning scrape...\n", n))

# Iterate rows by re-querying each time to avoid stale references
for (i in seq_len(n)) {
  Sys.sleep(0.3)
  rows <- tryCatch(remote_driver$findElements(using = "xpath", value = rows_xpath), error = function(e) list())
  if (length(rows) < i) break

  link_xpath <- sprintf("(%s)[%d]//td[2]//a", rows_xpath, i)
  link_el <- tryCatch(remote_driver$findElement(using = "xpath", value = link_xpath), error = function(e) NULL)
  if (is.null(link_el)) {
    cat(sprintf("Row %d: link not found, skipping.\n", i))
    next
  }

  title_text <- tryCatch(link_el$getElementText()[[1]], error = function(e) sprintf("row_%03d", i))
  safe_name <- sanitize_filename(title_text)
  out_file <- file.path(out_dir, paste0(safe_name, ".txt"))

  # Click to open panel
  try(link_el$click(), silent = TRUE)
  Sys.sleep(1.2)

  # Extract panel text (retry once if empty)
  panel_text <- tryCatch(extract_panel_text(remote_driver, timeout = 9000), error = function(e) "")
  if (!nzchar(panel_text)) {
    Sys.sleep(0.6)
    try(link_el$click(), silent = TRUE)
    Sys.sleep(1.2)
    panel_text <- tryCatch(extract_panel_text(remote_driver, timeout = 9000), error = function(e) "")
  }

  if (nzchar(panel_text)) {
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

  # Close panel best-effort
  close_panel_if_open(remote_driver)
  Sys.sleep(0.5)
}

cat("Done.\n")
