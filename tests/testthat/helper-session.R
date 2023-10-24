test_session <- function(verbose = FALSE) {
  skip_if_offline()
  skip_if_not(selenium_server_available())

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
  file <- normalizePath(testthat::test_path("helper-site.html"))

  if (grepl("^/tmp", file) || is_check()) {
    if (!is_check() && env_var_is_true("CI")) {
      dir <- rappdirs::user_data_dir("helper-site")
      dir.create(dir, recursive = TRUE)
      file.copy(file, file.path(dir, "helper-site.html"))
      file <- normalizePath(file.path(dir, "helper-site.html"))
      stopifnot(length(readLines(file)) > 1)
    } else {
      skip("Browsers cannot access HTML files in the temporary directory.")
    }
  }

  session <- test_session(verbose = verbose)

  session$navigate(paste0("file://", file))
  session
}

is_cran_check <- function() {
  if (env_var_is_true("NOT_CRAN")) {
    FALSE
  } else {
    is_check()
  }
}

is_check <- function() Sys.getenv("_R_CHECK_PACKAGE_NAME_", "") != ""
