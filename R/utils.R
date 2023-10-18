combine_lists <- function(x, y, depth = 1) {
  if (is.function(y)) {
    return(y(x))
  } else if (depth == 0 || is.null(x)) {
    return(y)
  }

  for (i in seq_along(y)) {
    nm <- names(y)[i]
    x[[nm]] <- combine_lists(x[[nm]], y[[i]], depth - 1)
  }

  x
}

named_list <- function() {
  res <- list()
  names(res) <- character()
  res
}

compact <- function(x) {
  x[!vapply(x, is_empty, logical(1))]
}

is_empty <- function(x) length(x) == 0

web_element_id <- "element-6066-11e4-a52e-4f735466cecf"

shadow_element_id <- "shadow-6066-11e4-a52e-4f735466cecf"

to_sentence_case <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

rand_id <- function() {
  as.character(round(stats::runif(1, min = 0, max = 1000000)))
}

get_server_status <- function(port = 4444L, host = "localhost", verbose = FALSE) {
  url <- sprintf("http://%s:%s", host, port)
  req <- httr2::request(url)
  get_status(req, verbose = verbose)
}

get_status <- function(req, verbose = FALSE) {
  req <- req_command(req, "Status")
  response <- req_perform_selenium(req, verbose = verbose)
  httr2::resp_body_json(response)$value
}

selenium_server_available <- function(port = 4444L, host = "localhost", verbose = FALSE) {
  tryCatch(
    get_server_status(port = port, host = host, verbose = verbose)$value,
    error = function(e) FALSE
  )
}
