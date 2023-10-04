#' @export
SeleniumSession <- R6::R6Class(
  "SeleniumSession",
  public = list(
    id = NULL,
    initialize = function(browser = "chrome", port = 4444L, host = "localhost", verbose = FALSE) {
      url <- sprintf("http://%s:%s", host, port)
      private$req <- httr2::request(url)
      private$verbose <- verbose

      req <- req_command(private$req, "New Session")

      body <- list(
        capabilities = list(
          firstMatch = list(named_list()),
          alwaysMatch = list(
            browserName = browser
          )
        )
      )
      req <- req_body_selenium(req, body)

      result <- req_perform_selenium(req, verbose = private$verbose)
      result_r <- httr2::resp_body_json(result)

      self$id <- result_r$value$sessionId

      invisible(self)
    },
    create_webelement = function(id) {
      result <- WebElement$new(self$id, private$req, private$verbose, id)
      result
    },
    create_shadowroot = function(id) {
      ShadowRoot$new(self$id, private$req, private$verbose, id)
    },
    close = function() {
      req <- req_command(private$req, "Delete Session", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    status = function() {
      req <- req_command(private$req, "Status")
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_timeouts = function() {
      req <- req_command(private$req, "Get Timeouts", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    set_timeouts = function(script = NULL, page_load = NULL, implicit_wait = NULL) {
      req <- req_command(private$req, "Set Timeouts", session_id = self$id)
      body <- compact(list(
        script = script,
        pageLoad = page_load,
        implicit = implicit_wait
      ))

      if (length(body) == 0) {
        stop()
      }

      req <- req_body_selenium(req, body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
      invisible(self)
    },
    navigate = function(url) {
      req <- req_command(private$req, "Navigate To", session_id = self$id)
      req <- req_body_selenium(req, list(url = url))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    current_url = function() {
      req <- req_command(private$req, "Get Current URL", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    back = function() {
      req <- req_command(private$req, "Back", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    forward = function() {
      req <- req_command(private$req, "Forward", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    refresh = function() {
      req <- req_command(private$req, "Refresh", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    title = function() {
      req <- req_command(private$req, "Get Title", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    window_handle = function() {
      req <- req_command(private$req, "Get Window Handle", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    close_window = function() {
      req <- req_command(private$req, "Close Window", session_id = self$id)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    switch_to_window = function(handle) {
      req <- req_command(private$req, "Switch To Window", session_id = self$id)
      req <- req_body_selenium(req, list(handle = handle))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    window_handles = function() {
      req <- req_command(private$req, "Get Window Handles", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    new_window = function(type = c("tab", "window")) {
      type <- rlang::arg_match(type)
      req <- req_command(private$req, "New Window", session_id = self$id)
      req <- req_body_selenium(req, list(type = type))
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    switch_to_frame = function(id = NA) {
      if (inherits(id, "WebElement")) {
        id <- id$toJSON()
      }

      req <- req_command(private$req, "Switch To Frame", session_id = self$id)
      req <- req_body_selenium(req, list(id = id))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    switch_to_parent_frame = function() {
      req <- req_command(private$req, "Switch To Parent Frame", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    get_window_rect = function() {
      req <- req_command(private$req, "Get Window Rect", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    set_window_rect = function(width = NULL, height = NULL, x = NULL, y = NULL) {
      req <- req_command(private$req, "Set Window Rect", session_id = self$id)
      body <- compact(list(
        width = width,
        height = height,
        x = x,
        y = y
      ))

      if (length(body) == 0) {
        stop()
      }

      req <- req_body_selenium(req, body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    maximize_window = function() {
      req <- req_command(private$req, "Maximize Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    minimize_window = function() {
      req <- req_command(private$req, "Minimize Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    fullscreen_window = function() {
      req <- req_command(private$req, "Fullscreen Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    get_active_element = function() {
      req <- req_command(private$req, "Get Active Element", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value
      self$create_webelement(id)
    },
    find_element = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                            value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Element", session_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value
      self$create_webelement(id[[1]])
    },
    find_elements = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                             value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Elements", session_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      ids <- httr2::resp_body_json(response)$value
      lapply(ids, function(x) self$create_webelement(x[[1]]))
    },
    get_page_source = function() {
      req <- req_command(private$req, "Get Page Source", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    execute_script = function(x, ...) {
      args <- rlang::list2(...)
      args <- prepare_for_json(args)
      req <- req_command(private$req, "Execute Script", session_id = self$id)
      req <- req_body_selenium(req, list(script = x, args = args))
      response <- req_perform_selenium(req, verbose = private$verbose)
      parse_json_result(httr2::resp_body_json(response)$value, self)
    },
    execute_async_script = function(x, ...) {
      args <- rlang::list2(...)
      args <- prepare_for_json(args)
      req <- req_command(private$req, "Execute Async Script", session_id = self$id)
      req <- req_body_selenium(req, list(script = x, args = args))
      req <- req_body_selenium(req, list(script = x, args = args))
      response <- req_perform_selenium(req, verbose = private$verbose)
      parse_json_result(httr2::resp_body_json(response)$value, self)
    },
    get_cookies = function() {
      req <- req_command(private$req, "Get All Cookies", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    get_cookie = function(name) {
      req <- req_command(private$req, "Get Named Cookie", session_id = self$id, name = name)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    add_cookie = function(cookie) {
      req <- req_command(private$req, "Add Cookie", session_id = self$id)
      req <- req_body_selenium(req, list(cookie = cookie))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    delete_cookie = function(name) {
      req <- req_command(private$req, "Delete Cookie", session_id = self$id, name = name)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    delete_all_cookies = function() {
      req <- req_command(private$req, "Delete All Cookies", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    perform_actions = function(actions, release_actions = TRUE) {
      actions <- unclass_stream(actions)
      req <- req_command(private$req, "Perform Actions", session_id = self$id)
      req <- req_body_selenium(req, list(actions = actions))
      req_perform_selenium(req, verbose = private$verbose)
      if (release_actions) {
        self$release_actions()
      }
      invisible(self)
    },
    release_actions = function() {
      req <- req_command(private$req, "Release Actions", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    dismiss_alert = function() {
      req <- req_command(private$req, "Dismiss Alert", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    accept_alert = function() {
      req <- req_command(private$req, "Accept Alert", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    get_alert_text = function() {
      req <- req_command(private$req, "Get Alert Text", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    send_alert_text = function(text) {
      req <- req_command(private$req, "Send Alert Text", session_id = self$id)
      req <- req_body_selenium(req, list(text = text))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    screenshot = function() {
      req <- req_command(private$req, "Take Screenshot", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    print_page = function(orientation = c("portrait", "landscape"),
                     scale = 1,
                     background = FALSE,
                     width = NULL,
                     height = NULL,
                     margin = NULL,
                     top = NULL,
                     left = NULL,
                     footer = NULL,
                     header = NULL,
                     shrink_to_fit = NULL,
                     page_ranges = NULL) {
      req <- req_command(private$req, "Print Page", session_id = self$id)
      body <- compact(list(
        orientation = rlang::arg_match(orientation),
        scale = scale,
        background = background,
        width = width,
        height = height,
        margin = margin,
        top = top,
        left = left,
        footer = footer,
        header = header,
        shrinkToFit = shrink_to_fit,
        pageRanges = page_ranges
      ))
      req <- req_body_selenium(req, body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    }
  ),
  private = list(
    req = NULL,
    verbose = NULL
  )
)
