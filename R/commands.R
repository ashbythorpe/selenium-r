execute_command(req, command, session_id = NULL, element_id = NULL) {
  url <- commands[command]
  if (!is.null(session_id)) {
    url <- gsub("\\{session_id\\}", session_id, url, fixed = TRUE)
  }
  if (!is.null(element_id)) {
    url <- gsub("\\{element_id\\}", element_id, url, fixed = TRUE)
  }

  httr2::req_url_path_append(url)
}
