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

req_perform_selenium <- function(req, verbose = FALSE, timeout = NULL, call = rlang::caller_env()) {
  if (verbose) {
    req <- httr2::req_verbose(req)
  }

  req <- httr2::req_error(req, body = extract_error_message)

  if (!is.null(timeout)) {
    req <- httr2::req_timeout(req, timeout)
  }

  rlang::local_error_call(call)

  httr2::req_perform(req)
}

extract_error_message <- function(resp) {
  if (httr2::resp_content_type(resp) != "application/json") {
    return(NULL)
  }

  body <- httr2::resp_body_json(resp)

  if (is.list(body$value)) {
    error <- body$value$error
    message <- body$value$message
  } else {
    error <- body$error
    message <- body$message
  }
  message <- gsub(paste0(error, ": "), "", message, fixed = TRUE)

  c(
    "x" = paste0(to_sentence_case(error), "."),
    "x" = indent_message(message)
  )
}

indent_message <- function(x) {
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  lines[-1] <- paste0("  ", lines[-1])
  paste(lines, collapse = "\n")
}
