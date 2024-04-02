#' Custom browser options
#'
#' Create browser options to pass into the `capabilities` argument of
#' [SeleniumSession$new()][SeleniumSession].
#'
#' @param binary Path to the browser binary.
#' @param arguments A character vector of additional arguments to pass to the
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
#' other languages (e.g. Java/Python) to R.
#'
#' You can combine these options with non-browser specific options simply using
#' [c()].
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
#' chrome_options(
#'   binary = "/path/to/chrome",
#'   arguments = c("--headless", "--disable-gpu"),
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
#' @export
chrome_options <- function(binary = NULL,
                           arguments = NULL,
                           extensions = NULL,
                           prefs = NULL,
                           ...) {
  check_string(binary, allow_null = TRUE)
  check_character(arguments, allow_null = TRUE)
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
      args = as.list(arguments),
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
                            arguments = NULL,
                            profile = NULL,
                            prefs = NULL,
                            ...) {
  check_string(binary, allow_null = TRUE)
  check_character(arguments, allow_null = TRUE)
  check_string(profile, allow_null = TRUE)
  check_list(prefs, allow_null = TRUE)

  if (!is.null(profile) && !inherits(profile, "AsIs")) {
    profile <- base64enc::base64encode(file = profile)
  }

  compact(list(
    acceptInsecureCerts = TRUE,
    `moz:firefoxOptions` = list(
      binary = binary,
      args = as.list(arguments),
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
                         arguments = NULL,
                         extensions = NULL,
                         prefs = NULL,
                         ...) {
  check_string(binary, allow_null = TRUE)
  check_character(arguments, allow_null = TRUE)
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
      args = as.list(arguments),
      extensions = extensions_encoded,
      prefs = prefs,
      ...
    )
  ))
}
