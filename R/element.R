#' @export
WebElement <- R6::R6Class("WebElement",
  public = list(
    id = NULL,
    initialize = function(session_id, req, verbose, id) {
      private$session_id <- session_id
      private$req <- req
      private$verbose <- verbose
      self$id <- id
    },
    shadow_root = function() {
      req <- req_command(private$req, "Get Element Shadow Root", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value[[1]]
      ShadowRoot$new(private$session_id, private$req, private$verbose, id)
    },
    find_element = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                            value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Element From Element", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value[[1]]
      WebElement$new(private$session_id, private$req, private$verbose, id)
    },
    find_elements = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                             value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Elements From Element", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      ids <- httr2::resp_body_json(response)$value
      lapply(ids, function(id) WebElement$new(private$session_id, private$req, private$verbose, id[[1]]))
    },
    is_selected = function() {
      req <- req_command(private$req, "Is Element Selected", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_attribute = function(name) {
      req <- req_command(private$req, "Get Element Attribute", session_id = private$session_id, element_id = self$id, name = name)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_property = function(name) {
      req <- req_command(private$req, "Get Element Property", session_id = private$session_id, element_id = self$id, name = name)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_css_value = function(name) {
      req <- req_command(private$req, "Get Element CSS Value", session_id = private$session_id, element_id = self$id, "property name" = name)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_text = function() {
      req <- req_command(private$req, "Get Element Text", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_tag_name = function() {
      req <- req_command(private$req, "Get Element Tag Name", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_rect = function() {
      req <- req_command(private$req, "Get Element Rect", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    is_enabled = function() {
      req <- req_command(private$req, "Is Element Enabled", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    computed_role = function() {
      req <- req_command(private$req, "Get Computed Role", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    computed_label = function() {
      req <- req_command(private$req, "Get Computed Label", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    click = function() {
      req <- req_command(private$req, "Element Click", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    clear = function() {
      req <- req_command(private$req, "Element Clear", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    send_keys = function(...) {
      req <- req_command(private$req, "Element Send Keys", session_id = private$session_id, element_id = self$id)
      body <- list(
        text = parse_keys(...)
      )
      req <- req_body_selenium(req, body)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    screenshot = function() {
      req <- req_command(private$req, "Take Element Screenshot", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    toJSON = function() {
      res <- list(self$id)
      names(res) <- web_element_id
      res
    }
  ),
  private = list(
    session_id = NULL,
    req = NULL,
    verbose = NULL
  )
)

#' @export
ShadowRoot <- R6::R6Class("ShadowRoot",
  public = list(
    id = NULL,
    initialize = function(session_id, req, verbose, id) {
      private$session_id <- session_id
      private$req <- req
      private$verbose <- verbose
      self$id <- id
    },
    find_element = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                            value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Element From Shadow Root", session_id = private$session_id, shadow_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value
      WebElement$new(private$session_id, private$req, private$verbose, id[[1]])
    },
    find_elements = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                             value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Elements From Shadow Root", session_id = private$session_id, shadow_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      ids <- httr2::resp_body_json(response)$value
      lapply(ids, function(id) WebElement$new(private$session_id, private$req, private$verbose, id[[1]]))
    },
    toJSON = function() {
      res <- list(self$id)
      names(res) <- shadow_element_id
      res
    }
  ),
  private = list(
    session_id = NULL,
    req = NULL,
    verbose = NULL
  )
)
