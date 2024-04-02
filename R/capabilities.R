#' Custom browser options
#'
#' Create browser options to pass into the `capabilities` argument of
#' [SeleniumSession$new()][SeleniumSession].
#'
#' @param binary Path to the browser binary.
#' @param args A character vector of additional arguments to pass to the
#'   browser.
#' @param extensions A character vector of paths to browser extension (`.crx`)
#'   files. These will be base64 encoded before being passed to the browser. If
#'   you have already  encoded the extensions, you can pass them using [I()].
#'   For Firefox, use a profile to load extensions.
#' @param prefs A named list of preferences to set in the browser.
#' @param profile Path to a Firefox profile directory. This will be base64
#'   encoded before being passed to the browser.
#' @param ... Additional options to pass to the browser.
#'
#' @details
#' These functions allow you to more easily translate between Selenium code in
#' other languages (e.g. Java/Python) to R. For example, consider the following
#' Java code, adapted from the the
#' [Selenium documentation](https://www.selenium.dev/documentation/webdriver/browsers/chrome/)
#'
#' ```java
#' ChromeOptions options = new ChromeOptions();
#'
#' options.setBinary("/path/to/chrome");
#' options.addArguments("--headless", "--disable-gpu");
#' options.addExtensions("/path/to/extension.crx");
#' options.setExperimentalOption("excludeSwitches", List.of("disable-popup-blocking"));
#' ```
#'
#' This can be translated to R as follows:
#'
#' ```r
#' chrome_options(
#'   binary = "/path/to/chrome",
#'   args = c("--headless", "--disable-gpu"),
#'   extensions = "/path/to/extension.crx",
#'   excludeSwitches = list("disable-popup-blocking")
#' )
#' ```
#'
#' You can combine these options with non-browser specific options simply using
#' [c()].
#'
#' Note that Microsoft Edge options are very similar to Chrome options, since
#' it is based on Chromium.
#'
#' @returns A list of browser options, with Chrome options under the name
#'   `goog:chromeOptions`, Firefox options under `moz:firefoxOptions`, and Edge
#'   options under `ms:edgeOptions`.
#'
#' @seealso
#' For more information and examples on Chrome options, see:
#' <https://chromedriver.chromium.org/capabilities>
#'
#' For Firefox options:
#' <https://developer.mozilla.org/en-US/docs/Web/WebDriver/Capabilities/firefoxOptions>
#'
#' For other options that affect Firefox but are not under `mox:firefoxOptions`,
#' see:
#' <https://firefox-source-docs.mozilla.org/testing/geckodriver/Capabilities.html>
#'
#' For Edge options, see:
#' <https://learn.microsoft.com/en-us/microsoft-edge/webdriver-chromium/capabilities-edge-options#edgeoptions-object>
#'
#' @examples
#' # Basic options objects
#' chrome_options(
#'   binary = "/path/to/chrome",
#'   args = c("--headless", "--disable-gpu"),
#'   detatch = TRUE, # An additional option described in the link above.
#'   prefs = list(
#'     "profile.default_content_setting_values.notifications" = 2
#'   )
#' )
#'
#'
#' firefox_options(binary = "/path/to/firefox")
#'
#' edge_options(binary = "/path/to/edge")
#'
#' # Setting the user agent
#' chrome_options(args = c("--user-agent=My User Agent"))
#'
#' edge_options(args = c("--user-agent=My User Agent"))
#'
#' firefox_options(prefs = list(
#'   "general.useragent.override" = "My User Agent"
#' ))
#'
#' # Using a proxy server
#'
#' chrome_options(args = c("--proxy-server=HOST:PORT"))
#'
#' edge_options(args = c("--proxy-server=HOST:PORT"))
#'
#' PORT <- 1
#' firefox_options(prefs = list(
#'   "network.proxy.type" = 1,
#'   "network.proxy.socks" = "HOST",
#'   "network.proxy.socks_port" = PORT,
#'   "network.proxy.socks_remote_dns" = FALSE
#' ))
#'
#' # Combining with other options
#' browser_options <- chrome_options(binary = "/path/to/chrome")
#'
#' c(browser_options, list(platformName = "Windows"))
#'
#' @export
chrome_options <- function(binary = NULL,
                           args = NULL,
                           extensions = NULL,
                           prefs = NULL,
                           ...) {
  check_string(binary, allow_null = TRUE)
  check_character(args, allow_null = TRUE)
  check_character(extensions, allow_null = TRUE)
  check_list(prefs, allow_null = TRUE)

  extensions_encoded <- if (is.null(extensions)) {
    NULL
  } else if (inherits(extensions, "AsIs")) {
    as.list(extensions)
  } else {
    lapply(extensions, function(x) {
      base64enc::base64encode(file = x)
    })
  }

  compact(list(
    `goog:chromeOptions` = list(
      binary = binary,
      args = as.list(args),
      extensions = extensions_encoded,
      prefs = prefs,
      ...
    )
  ))
}

#' @rdname chrome_options
#'
#' @export
firefox_options <- function(binary = NULL,
                            args = NULL,
                            profile = NULL,
                            prefs = NULL,
                            ...) {
  check_string(binary, allow_null = TRUE)
  check_character(args, allow_null = TRUE)
  check_string(profile, allow_null = TRUE)
  check_list(prefs, allow_null = TRUE)

  if (!is.null(profile) && !inherits(profile, "AsIs")) {
    profile <- base64enc::base64encode(file = profile)
  }

  compact(list(
    acceptInsecureCerts = TRUE,
    `moz:firefoxOptions` = list(
      binary = binary,
      args = as.list(args),
      profile = profile,
      ...
    ),
    `moz:debuggerAddress` = TRUE
  ))
}

#' @rdname chrome_options
#'
#' @export
edge_options <- function(binary = NULL,
                         args = NULL,
                         extensions = NULL,
                         prefs = NULL,
                         ...) {
  check_string(binary, allow_null = TRUE)
  check_character(args, allow_null = TRUE)
  check_character(extensions, allow_null = TRUE)
  check_list(prefs, allow_null = TRUE)

  extensions_encoded <- if (is.null(extensions)) {
    NULL
  } else if (inherits(extensions, "AsIs")) {
    as.list(extensions)
  } else {
    lapply(extensions, function(x) {
      base64enc::base64encode(file = x)
    })
  }

  compact(list(
    `ms:edgeOptions` = list(
      binary = binary,
      args = as.list(args),
      extensions = extensions_encoded,
      prefs = prefs,
      ...
    )
  ))
}
