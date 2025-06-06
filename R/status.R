#' Is a selenium server instance running?
#'
#' @description
#' `wait_for_server()` takes a server process returned by [selenium_server()]
#' and waits for it to respond to status requests. If it doesn't, then an
#' error is thrown detailing any errors in the response and any error messages
#' from the server.
#'
#' `selenium_server_available()` returns `TRUE` if a Selenium server is
#' running on a given port and host. `wait_for_selenium_available()` waits
#' for the Selenium server to become available for a given time, throwing an
#' error if one does not. It is similar to `wait_for_server()` except that it
#' works with servers not created by selenium.
#'
#' `get_server_status()`, when given a port and host, figures out whether a
#' Selenium server instance is running, and if so, returns its status. This is
#' used by `selenium_server_available()` to figure out if the server is
#' running.
#'
#' @param server The process object returned by [selenium_server()].
#' @param port The port that the Selenium server is using, so we can
#'   connect to it.
#' @param host The host that the Selenium server is running on. This is
#'   usually 'localhost' (i.e. Your own machine).
#' @param max_time The amount of time to wait for the Selenium server to
#'   become available.
#' @param error Whether to throw an error if the web request fails
#'   after the timeout is exceeded. If not, and we can't connect to a server,
#'   `FALSE` is returned.
#' @param verbose Whether to print information about the web request that is
#'   sent.
#' @param timeout How long to wait for a request to recieve a response before
#'   throwing an error.
#'
#' @returns
#' `wait_for_server()` and `wait_for_selenium_available()` return `TRUE` if
#' the server is ready to be connected to, and throw an error otherwise.
#'
#' `selenium_server_available()` returns `TRUE` if a Selenium server is
#' running, and `FALSE` otherwise.
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
#' server <- selenium_server()
#'
#' wait_for_server(server)
#'
#' get_server_status()
#'
#' selenium_server_available()
#'
#' wait_for_selenium_available()
#' }
#'
#' @export
wait_for_server <- function(server,
                            port = 4444L,
                            host = "localhost",
                            max_time = 60,
                            error = TRUE,
                            verbose = FALSE,
                            timeout = 20) {
  check_class(server, "process")
  check_number_decimal(timeout)
  check_number_whole(max_time)
  check_bool(verbose)
  check_bool(error)

  result <- wait_for_status(max_time, port, host, verbose, timeout)

  if (result) {
    return(TRUE)
  }

  if (error) {
    base_message <- "Timed out waiting for selenium server to start"

    parent <- if (rlang::is_error(result)) result else NULL

    rlang::abort(base_message, parent = parent)
  }

  FALSE
}

#' @rdname wait_for_server
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

#' @rdname wait_for_server
#'
#'
#' @export
wait_for_selenium_available <- function(max_time = 60,
                                        port = 4444L,
                                        host = "localhost",
                                        error = TRUE,
                                        verbose = FALSE,
                                        timeout = 20) {
  check_number_decimal(timeout)
  check_number_whole(port)
  check_string(host)
  check_bool(verbose)
  check_bool(error)

  result <- wait_for_status(max_time, port, host, verbose, timeout)

  if (result) {
    return(TRUE)
  }

  if (error) {
    parent <- if (rlang::is_error(result)) result else NULL

    rlang::abort("Timed out waiting for selenium server to start", parent = result)
  }

  FALSE
}


wait_for_status <- function(max_time, port, host, verbose, timeout) {
  end <- Sys.time() + timeout

  result <- rlang::try_fetch(
    get_server_status(port = port, host = host, verbose = verbose, timeout = timeout),
    error = identity
  )

  if (isTRUE(result$ready)) {
    return(TRUE)
  }

  while (Sys.time() <= end) {
    result <- rlang::try_fetch(
      get_server_status(port = port, host = host, verbose = verbose, timeout = timeout),
      error = identity
    )

    if (isTRUE(result$ready)) {
      return(TRUE)
    }
  }

  FALSE
}

get_status <- function(req, verbose = FALSE, timeout = 20) {
  req <- req_command(req, "Status")
  response <- req_perform_selenium(req, verbose = verbose, timeout = timeout)
  httr2::resp_body_json(response)$value
}

#' @rdname wait_for_server
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
