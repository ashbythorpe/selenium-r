#' Start a Selenium Client session
#'
#' @description
#' This class represents the client to a Selenium session. It will only work
#' if a Selenium server instance is running. If you get an error, use 
#' [selenium_server_available()] to check if a server is running. See the 
#' package README for more information, or use [selenium_server()] to try and
#' start a server automatically.
#'
#' @export
SeleniumSession <- R6::R6Class(
  "SeleniumSession",
  public = list(
    #' @field id The id of the session, generated when the session is started.
    id = NULL,

    #' @description
    #' Initialize a `SeleniumSession` object.
    #'
    #' @param browser The name of the browser to use (e.g. "chrome", "firefox",
    #'   "edge").
    #' @param port The port that the Selenium server is using, so we can
    #'   connect to it.
    #' @param host The host that the Selenium server is running on. This is
    #'   usually 'localhost' (i.e. Your own machine).
    #' @param verbose Whether to print the web requests that are being sent and
    #'   any responses.
    #'
    #' @returns A `SeleniumSession` object.
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
    },
    #' @description
    #' Create a `WebElement` object using the parameters of the current
    #' session.
    #'
    #' @param id The element id.
    #'
    #' @returns A `WebElement` object.
    create_webelement = function(id) {
      result <- WebElement$new(self$id, private$req, private$verbose, id)
      result
    },

    #' @description
    #' Create a `ShadowRoot` object using the parameters of the current
    #' session.
    #'
    #' @param id The shadow root id.
    #'
    #' @returns A `ShadowRoot` object.
    create_shadowroot = function(id) {
      ShadowRoot$new(self$id, private$req, private$verbose, id)
    },

    #' @description
    #' Close the current session. Once a session is closed, its methods will
    #' no longer work. However, the Selenium server will still be running.
    #'
    #' @returns The session object, invisibly.
    close = function() {
      req <- req_command(private$req, "Delete Session", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the status of the Selenium server. Unlike all other methods, this
    #' method is independent of the session itself (meaning it can be used
    #' even after `SeleniumSession$close()` is called). It is identical to
    #' [get_server_status()], but uses the host, port and verbose options
    #' passed to the session, for convenience.
    #'
    #' @returns A list that can (but may not always) contain the following
    #'   fields:
    #'
    #' * `ready`: Whether the server is ready to be connected to. This should
    #'     always be returned by the server.
    #' * `message`: A message about the status of the server.
    #' * `uptime`: How long the server has been running.
    #' * `nodes`: Information about the slots that the server can take.
    status = function() {
      get_status(private$req, private$verbose)
    },
    #' @description
    #' Get the timeouts of the current session. There are three types of
    #' timeouts:
    #'
    #' * *session script timeout*: The amount of time that the server will wait
    #'    for scripts to run. Defaults to 3 seconds.
    #' * *page load timeout*: The amount of time that the server will wait for
    #'    the page to load. Defaults to 30 seconds.
    #' * *implicit wait*: The amount of time that the server will wait for
    #'    elements to be located, or for elements to become interactable when
    #'    required. Defaults to 0 seconds.
    #'
    #' @returns A list with three items: `script`, `page_load`, and `implicit`.
    get_timeouts = function() {
      req <- req_command(private$req, "Get Timeouts", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Set the timeouts of the current session. The types of timeouts are
    #' defined in `SeleniumSession$get_timeouts()`.
    #'
    #' @param script The amount of time to wait for scripts. By default, this
    #'   is not set.
    #' @param page_load The amount of time to wait for the page to load.
    #' @param implicit_wait The amount of time to wait for elements on the
    #'   page.
    #'
    #' @returns The session object, invisibly.
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
    #' @description
    #' Navigate to a URL.
    #'
    #' @param url The URL to navigate to. Must begin with a protocol (e.g. 
    #'   'https://').
    #'
    #' @returns The session object, invisibly.
    navigate = function(url) {
      req <- req_command(private$req, "Navigate To", session_id = self$id)
      req <- req_body_selenium(req, list(url = url))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the current URL.
    #'
    #' @returns The URL of the current page.
    current_url = function() {
      req <- req_command(private$req, "Get Current URL", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Go back in the navigation history.
    #'
    #' @returns The session object, invisibly.
    back = function() {
      req <- req_command(private$req, "Back", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Go forward in the navigation history.
    #'
    #' @returns The session object, invisibly.
    forward = function() {
      req <- req_command(private$req, "Forward", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Reload the current page.
    #'
    #' @returns The session object, invisibly.
    refresh = function() {
      req <- req_command(private$req, "Refresh", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the title of the current page.
    #'
    #' @returns The title of the current page.
    title = function() {
      req <- req_command(private$req, "Get Title", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the current window handle.
    #'
    #' @returns The handle of the current window (a string).
    window_handle = function() {
      req <- req_command(private$req, "Get Window Handle", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Close the current window.
    #'
    #' @returns The session object, invisibly.
    close_window = function() {
      req <- req_command(private$req, "Close Window", session_id = self$id)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Switch to a specific window.
    #'
    #' @param handle The handle of the window to switch to.
    #'
    #' @returns The session object, invisibly.
    switch_to_window = function(handle) {
      req <- req_command(private$req, "Switch To Window", session_id = self$id)
      req <- req_body_selenium(req, list(handle = handle))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the handles of all open windows.
    #'
    #' @returns The handles of all open windows (a list of strings).
    window_handles = function() {
      req <- req_command(private$req, "Get Window Handles", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Create a new window. Note that this window is not automatically
    #' switched to.
    #'
    #' @param type Whether to create a tab or a window.
    #'
    #' @returns A list containing two elements:
    #'
    #' * `handle`: The handle of the new window.
    #' * `type`: The type of window. ("tab" or "window").
    new_window = function(type = c("tab", "window")) {
      type <- rlang::arg_match(type)
      req <- req_command(private$req, "New Window", session_id = self$id)
      req <- req_body_selenium(req, list(type = type))
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Frames allow you to split a window into multiple sections, where each
    #' section can load a separate HTML document. This function allows you to
    #' switch to a specific frame, given its ID, meaning that frame will become
    #' the current browsing context.
    #'
    #' @param id The ID of the frame to switch to. By default, the top-level
    #'   browsing context is switched to (i.e. not a frame). This can also be
    #'   a `WebElement` object, in which case the frame that contains said
    #'   element will be switched to.
    #'
    #' @returns The session object, invisibly.
    switch_to_frame = function(id = NA) {
      if (inherits(id, "WebElement")) {
        id <- id$toJSON()
      }

      req <- req_command(private$req, "Switch To Frame", session_id = self$id)
      req <- req_body_selenium(req, list(id = id))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Switch to the parent frame of the current frame.
    #'
    #' @returns The session object, invisibly.
    switch_to_parent_frame = function() {
      req <- req_command(private$req, "Switch To Parent Frame", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the size and position of the current window.
    #'
    #' @returns A list containing four elements:
    #'
    #' * `x`: The x position of the window relative to the left of the screen.
    #' * `y`: The y position of the window relative to the top of the screen.
    #' * `width`: The width of the window.
    #' * `height`: The height of the window.
    get_window_rect = function() {
      req <- req_command(private$req, "Get Window Rect", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Set the size and position of the current window.
    #'
    #' @param width The width of the window.
    #' @param height The height of the window.
    #' @param x The x position of the window relative to the left of the screen.
    #' @param y The y position of the window relative to the top of the screen.
    #'
    #' @returns The session object, invisibly.
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
    #' @description
    #' Maximize the current window. This makes the window the maximum size it
    #' can be, without being full screen
    #'
    #' @returns The session object, invisibly.
    maximize_window = function() {
      req <- req_command(private$req, "Maximize Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Minimize the current window. This hides the window.
    #'
    #' @returns The session object, invisibly.
    minimize_window = function() {
      req <- req_command(private$req, "Minimize Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Makes the window full screen
    #'
    #' @returns The session object, invisibly.
    fullscreen_window = function() {
      req <- req_command(private$req, "Fullscreen Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Get the currently active element.
    #'
    #' @returns A `WebElement` object.
    get_active_element = function() {
      req <- req_command(private$req, "Get Active Element", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value
      self$create_webelement(id)
    },
    #' @description
    #' Find the first element matching a selector.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #'
    #' @returns A `WebElement` object.
    find_element = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                            value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Element", session_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value
      self$create_webelement(id[[1]])
    },
    #' @description
    #' Find all elements matching a selector.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #'
    #' @returns A list of `WebElement` objects.
    find_elements = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                             value) {
      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Elements", session_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value))
      response <- req_perform_selenium(req, verbose = private$verbose)
      ids <- httr2::resp_body_json(response)$value
      lapply(ids, function(x) self$create_webelement(x[[1]]))
    },
    #' @description
    #' Get the HTML source of the current page, serialized as a string.
    #'
    #' @returns A string.
    get_page_source = function() {
      req <- req_command(private$req, "Get Page Source", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Execute a JavaScript script.
    #'
    #' @param x The script to execute. To return a value, do so explicitly,
    #'   e.g. `return 1`.
    #' @param ... Additional arguments to pass to the script. These can be
    #' accessed in the script using the `arguments` array. Can be `WebElement`
    #' objects or lists of such objects, which will be converted to nodes.
    #'
    #' @returns The return value of the script. Nodes or lists of nodes will
    #'   be converted to `WebElement` objects.
    execute_script = function(x, ...) {
      args <- rlang::list2(...)
      args <- prepare_for_json(args)
      req <- req_command(private$req, "Execute Script", session_id = self$id)
      req <- req_body_selenium(req, list(script = x, args = args))
      response <- req_perform_selenium(req, verbose = private$verbose)
      parse_json_result(httr2::resp_body_json(response)$value, self)
    },
    #' @description
    #' Execute an asynchronous JavaScript script, waiting for a value to be
    #' returned.
    #'
    #' @param x The script to execute. Unlike `execute_script()`. You return
    #'   an value using the callback function, which can be accessed using
    #'   `arguments[arguments.length - 1]`. For example, to return 1, you
    #'   would write `arguments[arguments.length - 1](1)`. This allows you to
    #'   write asynchronous JavaScript, but treat it like synchronous R code.
    #' @param ... Additional arguments to pass to the script. Can be
    #' `WebElement` objects or lists of such objects, which will be converted
    #' to nodes.
    #'
    #' @returns The return value of the script. Nodes or lists of nodes will
    #'   be converted to `WebElement` objects.
    execute_async_script = function(x, ...) {
      args <- rlang::list2(...)
      args <- prepare_for_json(args)
      req <- req_command(private$req, "Execute Async Script", session_id = self$id)
      req <- req_body_selenium(req, list(script = x, args = args))
      req <- req_body_selenium(req, list(script = x, args = args))
      response <- req_perform_selenium(req, verbose = private$verbose)
      parse_json_result(httr2::resp_body_json(response)$value, self)
    },
    #' @description
    #' Get all cookies.
    #'
    #' @returns A list of cookies. Each cookie is a list with a `name` and
    #'   `value` field, along with some other optional fields.
    get_cookies = function() {
      req <- req_command(private$req, "Get All Cookies", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get a specific cookie using its name.
    #'
    #' @param name The name of the cookie.
    #'
    #' @returns The cookie object.
    get_cookie = function(name) {
      req <- req_command(private$req, "Get Named Cookie", session_id = self$id, name = name)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Add a cookie to the cookie store of the current document.
    #'
    #' @param cookie The cookie object to add: a list which must contain a
    #'   `name` and `value` field.
    #'
    #' @returns The session object, invisibly.
    add_cookie = function(cookie) {
      req <- req_command(private$req, "Add Cookie", session_id = self$id)
      req <- req_body_selenium(req, list(cookie = cookie))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Delete a cookie using its name.
    #'
    #' @param name The name of the cookie.
    #'
    #' @returns The session object, invisibly.
    delete_cookie = function(name) {
      req <- req_command(private$req, "Delete Cookie", session_id = self$id, name = name)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Delete all cookies in the cookie store of the current document.
    #'
    #' @returns The session object, invisibly.
    delete_all_cookies = function() {
      req <- req_command(private$req, "Delete All Cookies", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Perform a sequence of actions.
    #'
    #' @param actions A `selenium_actions_stream` object, created using
    #'   [actions_stream()].
    #' @param release_actions Whether to call `release_actions()` after
    #'   performing the actions.
    #'
    #' @returns The session object, invisibly.
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
    #' @description
    #' Release all keys and pointers that were pressed using
    #' `perform_actions()`.
    #'
    #' @returns The session object, invisibly.
    release_actions = function() {
      req <- req_command(private$req, "Release Actions", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Dismiss the current alert, if present.
    #'
    #' @returns The session object, invisibly.
    dismiss_alert = function() {
      req <- req_command(private$req, "Dismiss Alert", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Accept the current alert, if present.
    #'
    #' @returns The session object, invisibly.
    accept_alert = function() {
      req <- req_command(private$req, "Accept Alert", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the message of the current alert, if present.
    #'
    #' @returns The message of the current alert (a string).
    get_alert_text = function() {
      req <- req_command(private$req, "Get Alert Text", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Send text to the current alert, if present. Useful if the alert is a
    #' prompt.
    #'
    #' @param text The text to send.
    #'
    #' @returns The session object, invisibly.
    send_alert_text = function(text) {
      req <- req_command(private$req, "Send Alert Text", session_id = self$id)
      req <- req_body_selenium(req, list(text = text))
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Take a screenshot of the current page.
    #'
    #' @returns The base64-encoded PNG screenshot, as a string.
    screenshot = function() {
      req <- req_command(private$req, "Take Screenshot", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Render the current page as a PDF.
    #'
    #' @param orientation The page orientation, either `"portrait"` or
    #'   `"landscape"`.
    #' @param scale The page scale, a number between 0 and 1.
    #' @param background Whether to print the background of the page.
    #' @param width The page width, in inches.
    #' @param height The page height, in inches.
    #' @param margin The page margin, in inches. Either a number, in which case
    #'   the margin on all sides are set to that value, or a list of four
    #'   numbers, with names `left`, `right`, `top`, and `bottom`, in which
    #'   case the margin on each side is set individually.
    #' @param footer The page footer, as a string.
    #' @param header The page header, as a string.
    #' @param shrink_to_fit Whether to shrink the page to fit the width and
    #'   height.
    #' @param page_ranges A list of page ranges (e.g. `"1"`, `"1-3"`) to print.
    #'
    #' @returns The base64-encoded PDF, as a string.
    print_page = function(orientation = c("portrait", "landscape"),
                          scale = 1,
                          background = FALSE,
                          width = NULL,
                          height = NULL,
                          margin = NULL,
                          footer = NULL,
                          header = NULL,
                          shrink_to_fit = NULL,
                          page_ranges = NULL) {
      req <- req_command(private$req, "Print Page", session_id = self$id)

      if (!is.list(margin) && is.numeric(margin)) {
        margin <- list(
          left = margin,
          right = margin,
          top = margin,
          bottom = margin
        )
      }

      body <- compact(list(
        orientation = rlang::arg_match(orientation),
        scale = scale,
        background = background,
        page = list(
          width = width,
          height = height
        ),
        margin = margin,
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
