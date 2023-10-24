req_command <- function(req, command, session_id = NULL, element_id = NULL, shadow_id = NULL, ...) {
  stopifnot(command %in% names(commands))
  command_list <- commands[[command]]
  method <- command_list$method
  url <- command_list$url
  if (!is.null(session_id)) {
    url <- gsub("{session id}", session_id, url, fixed = TRUE)
  }
  if (!is.null(element_id)) {
    url <- gsub("{element id}", element_id, url, fixed = TRUE)
  }
  if (!is.null(shadow_id)) {
    url <- gsub("{shadow id}", shadow_id, url, fixed = TRUE)
  }

  extra_elements <- rlang::list2(...)
  for (a in names(extra_elements)) {
    url <- gsub(paste0("{", a, "}"), extra_elements[[a]], url, fixed = TRUE)
  }

  httr2::req_method(httr2::req_url_path_append(req, url), method)
}

req_body_selenium <- function(req, body, request_body = NULL) {
  if (!is.null(request_body)) {
    body <- request_body
  }

  body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  req <- httr2::req_body_raw(req, body)
  req <- httr2::req_headers(
    req,
    "Content-Type" = "application/json; charset=utf-8",
    "Accept" = "application/json; charset=utf-8"
  )
  req
}

req_perform_selenium <- function(req, verbose = FALSE, call = rlang::caller_env()) {
  if (verbose) {
    req <- httr2::req_verbose(req)
  }

  rlang::try_fetch(
    httr2::req_perform(req),
    httr2_http = function(e) {
      handle_error(e, call = call)
    }
  )
}

handle_error <- function(x, call = rlang::caller_env()) {
  value <- httr2::resp_body_json(x$resp)
  if (is.list(value$value)) {
    error <- value$value$error
    message <- value$value$message
  } else {
    error <- value$error
    message <- value$message
  }
  message <- gsub(paste0(error, ": "), "", message, fixed = TRUE)

  rlang::abort(
    c(
      paste0(to_sentence_case(error), "."),
      "x" = message
    ),
    class = "selenium_error",
    call = call,
    parent = x,
    data = value,
    code = error
  )
}
