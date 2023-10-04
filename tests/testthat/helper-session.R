test_session <- function(verbose = FALSE) {
  skip_if(is_cran_check())
  browser <- Sys.getenv("SELENIUM_BROWSER", "chrome")
  port <- as.integer(Sys.getenv("SELENIUM_PORT", 4444L))
  host <- Sys.getenv("SELENIUM_HOST", "localhost")

  session <- try(SeleniumSession$new(browser = browser, port = port, host = host, verbose = verbose))
  if (inherits(session, "try-error")) {
    skip("Selenium session failed to start: You must have a Selenium server running.")
  }
  session
}

test_helper_site <- function(verbose = FALSE) {
  session <- test_session(verbose = verbose)

  file <- normalizePath(testthat::test_path("helper-site.html"))
  session$navigate(paste0("file://", file))
  session
}

is_cran_check <- function() {
  if (isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
    FALSE
  } else {
    is_check()
  }
}

is_check <- function() Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
