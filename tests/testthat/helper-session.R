test_session <- function(verbose = FALSE) {
  skip_if_offline()
  skip_if(!selenium_server_available())

  browser <- Sys.getenv("SELENIUM_BROWSER", "chrome")
  port <- as.integer(Sys.getenv("SELENIUM_PORT", 4444L))
  host <- Sys.getenv("SELENIUM_HOST", "localhost")

  opts <- if (browser == "chrome") {
    list(`goog:chromeOptions` = list(
      args = list(
        "remote-debugging-port=9222"
      )
    ))
  } else {
    NULL
  }

  session <- try(SeleniumSession$new(
    browser = browser,
    port = port,
    host = host,
    verbose = verbose,
    capabilities = opts
  ))

  if (inherits(session, "try-error")) {
    skip("Selenium session failed to start: You must have a Selenium server running.")
  }

  session
}

test_helper_site <- function(verbose = FALSE) {
  skip_if(
    is_check(),
    "Browsers cannot access HTML files in local tempfiles"
  )

  session <- test_session(verbose = verbose)

  browser <- Sys.getenv("SELENIUM_BROWSER", "chrome")

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
