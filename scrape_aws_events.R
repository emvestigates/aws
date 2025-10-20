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
