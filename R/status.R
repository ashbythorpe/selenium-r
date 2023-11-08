#' Is a selenium server instance running?
#'
#' @description
#' `selenium_server_available()` returns `TRUE` if a Selenium server is
#' running on a given port and host. `wait_for_selenium_available()` waits
#' for the Selenium server to become available for a given time.
#'
#' `get_server_status()`, when given a port and host, figures out whether a
#' Selenium server instance is running, and if so, returns its status. This is
#' used by `selenium_server_available()` to figure out if the server is
#' running.
#'
#' @param port The port that the Selenium server is using, so we can
#'   connect to it.
#' @param host The host that the Selenium server is running on. This is
#'   usually 'localhost' (i.e. Your own machine).
#' @param verbose Whether to print information about the web request that is
#'   sent.
#' @param timeout How long to wait for a request to recieve a response before
#'   throwing an error.
#'
#' @returns
#' `selenium_server_available()` returns `TRUE` if a Selenium server is
#' running, and `FALSE` otherwise, and `wait_for_selenium_available()` returns
#' the same thing.
#'
#' `get_server_status()` returns a list that can (but may not always) contain
#' the following fields:
#'
#' * `ready`: Whether the server is ready to be connected to. This should
#'   always be returned by the server.
#' * `message`: A message about the status of the server.
#' * `uptime`: How long the server has been running.
#' * `nodes`: Information about the slots that the server can take.
#'
#' @examples
#' \dontrun{
#' get_server_status()
#'
#' selenium_server_available()
#'
#' wait_for_selenium_available()
#' }
#'
#' @export
selenium_server_available <- function(port = 4444L, host = "localhost", verbose = FALSE, timeout = 20) {
  check_number_whole(port)
  check_string(host)
  check_bool(verbose)

  tryCatch(
    isTRUE(get_server_status(port = port, host = host, verbose = verbose)$ready),
    error = function(e) FALSE
  )
}

#' @rdname selenium_server_available
#'
#' @param max_time The amount of time to wait for the Selenium server to
#'   become available.
#' @param error Whether to throw an error if the web request throws an error
#'   after the timeout is exceeded. By default, a logical value is always
#'   returned.
#'
#' @export
wait_for_selenium_available <- function(max_time = 60,
                                        port = 4444L,
                                        host = "localhost",
                                        verbose = FALSE,
                                        timeout = 20,
                                        error = FALSE) {
  check_number_decimal(timeout)
  check_number_whole(port)
  check_string(host)
  check_bool(verbose)
  check_bool(error)
  end <- Sys.time() + timeout

  while (Sys.time() <= end) {
    result <- rlang::try_fetch(
      get_server_status(port = port, host = host, verbose = verbose, timeout = timeout),
      error = identity
    )

    if (isTRUE(result$ready)) {
      return(TRUE)
    }
  }

  if (error && rlang::is_error(result)) {
    rlang::abort("Timed out waiting for selenium server to start", parent = result)
  }

  FALSE
}

get_status <- function(req, verbose = FALSE, timeout = 20) {
  req <- req_command(req, "Status")
  response <- req_perform_selenium(req, verbose = verbose, timeout = timeout)
  httr2::resp_body_json(response)$value
}

#' @rdname selenium_server_available
#'
#' @export
get_server_status <- function(port = 4444L, host = "localhost", verbose = FALSE, timeout = 20) {
  check_number_whole(port)
  check_string(host)
  check_bool(verbose)

  url <- sprintf("http://%s:%s", host, port)
  req <- httr2::request(url)
  get_status(req, verbose = verbose, timeout = timeout)
}
