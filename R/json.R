prepare_for_json <- function(x) {
  if (rlang::inherits_any(x, c("WebElement", "ShadowRoot"))) {
    x$toJSON()
  } else if (rlang::is_bare_list(x)) {
    lapply(x, prepare_for_json)
  } else {
    x
  }
}

parse_json_result <- function(x, self) {
  if (rlang::is_bare_list(x) && length(x) == 1 && names(x) %in% c(web_element_id, shadow_element_id)) {
    if (names(x) == web_element_id) {
      self$create_webelement(x[[1]])
    } else {
      self$create_shadowroot(x[[1]])
    }
  } else if (rlang::is_bare_list(x)) {
    lapply(x, parse_json_result, self)
  } else {
    x
  }
}
