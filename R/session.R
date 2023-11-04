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

    #' @field browser The browser that the session is using.
    browser = NULL,

    #' @field port The port that the session is using.
    port = NULL,

    #' @field host The host that the session is running on.
    host = NULL,

    #' @description
    #' Create a Selenium session: opening a browser which can be controlled by
    #' the Selenium client.
    #'
    #' @param browser The name of the browser to use (e.g. "chrome", "firefox",
    #'   "edge").
    #' @param port The port that the Selenium server is using, so we can
    #'   connect to it.
    #' @param host The host that the Selenium server is running on. This is
    #'   usually 'localhost' (i.e. your own machine).
    #' @param verbose Whether to print the web requests that are being sent and
    #'   any responses.
    #' @param capabilities A list of capabilities to pass to the Selenium
    #'   server, to combine with the defaults generated using `browser`.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server. Overrides `capabilities`.
    #'
    #' @return A `SeleniumSession` object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new(verbose = TRUE)
    #'
    #' session$close()
    #' }
    initialize = function(
      browser = "firefox",
      port = 4444L,
      host = "localhost",
      verbose = FALSE,
      capabilities = NULL,
      request_body = NULL
    ) {
      check_string(browser)
      check_number_whole(port)
      check_string(host)
      check_bool(verbose)
      check_list(capabilities, allow_null = TRUE)
      check_list(request_body, allow_null = TRUE)

      opts <- switch(browser,
        firefox = list(
          browserName = "firefox",
          acceptInsecureCerts = TRUE
        ),
        edge = list(
          browserName = "microsoftEdge",
        ),
        list(browserName = browser)
      )

      if (!is.null(capabilities)) {
        opts <- merge_lists(opts, capabilities)
      }

      body <- list(
        capabilities = list(
          firstMatch = list(named_list()),
          alwaysMatch = opts
        )
      )

      url <- sprintf("http://%s:%s", host, port)
      private$req <- httr2::request(url)
      private$verbose <- verbose

      req <- req_command(private$req, "New Session")

      req <- req_body_selenium(req, body, request_body = request_body)

      result <- req_perform_selenium(req, verbose = private$verbose)
      result_r <- httr2::resp_body_json(result)

      self$id <- result_r$value$sessionId
      self$browser <- browser
      self$port <- port
      self$host <- host
    },

    #' @description
    #' Create a [WebElement] object using the parameters of the current
    #' session.
    #'
    #' @param id The element id.
    #'
    #' @return A [WebElement] object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' element <- session$find_element(using = "css selector", value = "*")
    #'
    #' element2 <- session$create_webelement(id = element$id)
    #'
    #' session$close()
    #' }
    create_webelement = function(id) {
      check_string(id)

      result <- WebElement$new(self$id, private$req, private$verbose, id)
      result
    },

    #' @description
    #' Create a [ShadowRoot] object using the parameters of the current
    #' session.
    #'
    #' @param id The shadow root id.
    #'
    #' @return A [ShadowRoot] object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' shadow_root <- session$create_shadowroot(id = "foo")
    #'
    #' session$close()
    #' }
    create_shadowroot = function(id) {
      check_string(id)

      ShadowRoot$new(self$id, private$req, private$verbose, id)
    },

    #' @description
    #' Close the current session. Once a session is closed, its methods will
    #' no longer work. However, the Selenium server will still be running.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$close()
    #' }
    close = function() {
      req <- req_command(private$req, "Delete Session", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },

    #' @description
    #' Get the status of the Selenium server. Unlike all other methods, this
    #' method is independent of the session itself (meaning it can be used
    #' even after [SeleniumSession$close()][SeleniumSession] is called). It is
    #' identical to [get_server_status()], but uses the host, port and verbose
    #' options passed to the session, for convenience.
    #'
    #' @return A list that can (but may not always) contain the following
    #'   fields:
    #'
    #' * `ready`: Whether the server is ready to be connected to. This should
    #'     always be returned by the server.
    #' * `message`: A message about the status of the server.
    #' * `uptime`: How long the server has been running.
    #' * `nodes`: Information about the slots that the server can take.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$status()
    #'
    #' session$close()
    #'
    #' session$status()
    #' }
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
    #' @return A list with three items: `script`, `page_load`, and `implicit`.
    #'
    #' @examples
    #' \dontrun{
    #'
    #' session <- SeleniumSession$new()
    #'
    #' session$get_timeouts()
    #'
    #' session$close()
    #' }
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
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$set_timeouts(script = 100)
    #'
    #' session$get_timeouts()
    #'
    #' session$close()
    #' }
    set_timeouts = function(script = NULL, page_load = NULL, implicit_wait = NULL, request_body = NULL) {
      check_number_decimal(script, allow_null = TRUE)
      check_number_decimal(page_load, allow_null = TRUE)
      check_number_decimal(implicit_wait, allow_null = TRUE)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Set Timeouts", session_id = self$id)
      body <- compact(list(
        script = script,
        pageLoad = page_load,
        implicit = implicit_wait
      ))

      if (length(body) == 0) {
        stop()
      }

      req <- req_body_selenium(req, body, request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
      invisible(self)
    },
    #' @description
    #' Navigate to a URL.
    #'
    #' @param url The URL to navigate to. Must begin with a protocol (e.g.
    #'   'https://').
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$close()
    #' }
    navigate = function(url, request_body = NULL) {
      check_string(url)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Navigate To", session_id = self$id)
      req <- req_body_selenium(req, list(url = url), request_body = request_body)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the current URL.
    #'
    #' @return The URL of the current page.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$current_url()
    #'
    #' session$close()
    #' }
    current_url = function() {
      req <- req_command(private$req, "Get Current URL", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },

    #' @description
    #' Go back in the navigation history.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$navigate("https://www.tidyverse.org")
    #'
    #' session$back()
    #'
    #' session$current_url()
    #'
    #' session$close()
    #' }
    back = function() {
      req <- req_command(private$req, "Back", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },

    #' @description
    #' Go forward in the navigation history.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$navigate("https://www.tidyverse.org")
    #'
    #' session$back()
    #'
    #' session$forward()
    #'
    #' session$current_url()
    #'
    #' session$close()
    #' }
    forward = function() {
      req <- req_command(private$req, "Forward", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },

    #' @description
    #' Reload the current page.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$refresh()
    #'
    #' session$close()
    #' }
    refresh = function() {
      req <- req_command(private$req, "Refresh", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the title of the current page.
    #'
    #' @return The title of the current page.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$title()
    #'
    #' session$close()
    #' }
    title = function() {
      req <- req_command(private$req, "Get Title", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the current window handle.
    #'
    #' @return The handle of the current window (a string).
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$window_handle()
    #'
    #' session$close()
    #' }
    window_handle = function() {
      req <- req_command(private$req, "Get Window Handle", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Close the current window.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$new_window()
    #'
    #' session$close_window()
    #'
    #' session$close()
    #' }
    close_window = function() {
      req <- req_command(private$req, "Close Window", session_id = self$id)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Switch to a specific window.
    #'
    #' @param handle The handle of the window to switch to.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' handle <- session$window_handle()
    #'
    #' handle2 <- session$new_window()$handle
    #'
    #' session$switch_to_window(handle)
    #'
    #' session$switch_to_window(handle2)
    #'
    #' session$close()
    #' }
    switch_to_window = function(handle, request_body = NULL) {
      check_string(handle)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Switch To Window", session_id = self$id)
      req <- req_body_selenium(req, list(handle = handle), request_body = request_body)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the handles of all open windows.
    #'
    #' @return The handles of all open windows (a list of strings).
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' handles <- session$window_handles()
    #'
    #' session$close()
    #' }
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
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return A list containing two elements:
    #'
    #' * `handle`: The handle of the new window.
    #' * `type`: The type of window. ("tab" or "window").
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' handle <- session$new_window()$handle
    #'
    #' session$switch_to_window(handle)
    #'
    #' session$close()
    #' }
    new_window = function(type = c("tab", "window"), request_body = NULL) {
      type <- rlang::arg_match(type)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "New Window", session_id = self$id)
      req <- req_body_selenium(req, list(type = type), request_body = request_body)
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
    #'   a [WebElement] object, in which case the frame that contains said
    #'   element will be switched to.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$switch_to_frame()
    #'
    #' session$close()
    #' }
    switch_to_frame = function(id = NA, request_body = NULL) {
      check_list(request_body, allow_null = TRUE)

      if (inherits(id, "WebElement")) {
        id <- id$toJSON()
      } else {
        check_number_whole(id, allow_na = TRUE)
      }

      req <- req_command(private$req, "Switch To Frame", session_id = self$id)
      req <- req_body_selenium(req, list(id = id), request_body = request_body)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Switch to the parent frame of the current frame.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$switch_to_frame()
    #'
    #' session$switch_to_parent_frame()
    #'
    #' session$close()
    #' }
    switch_to_parent_frame = function() {
      req <- req_command(private$req, "Switch To Parent Frame", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the size and position of the current window.
    #'
    #' @return A list containing four elements:
    #'
    #' * `x`: The x position of the window relative to the left of the screen.
    #' * `y`: The y position of the window relative to the top of the screen.
    #' * `width`: The width of the window.
    #' * `height`: The height of the window.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$get_window_rect()
    #'
    #' session$close()
    #' }
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
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$set_window_rect(width = 800, height = 600, x = 2, y = 3)
    #'
    #' session$close()
    #' }
    set_window_rect = function(width = NULL, height = NULL, x = NULL, y = NULL, request_body = NULL) {
      check_number_decimal(width, min = 0)
      check_number_decimal(height, min = 0)
      check_number_decimal(x)
      check_number_decimal(y)
      check_list(request_body, allow_null = TRUE)

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

      req <- req_body_selenium(req, body, request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Maximize the current window. This makes the window the maximum size it
    #' can be, without being full screen
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$maximize_window()
    #'
    #' session$close()
    #' }
    maximize_window = function() {
      req <- req_command(private$req, "Maximize Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Minimize the current window. This hides the window.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$minimize_window()
    #'
    #' session$close()
    #' }
    minimize_window = function() {
      req <- req_command(private$req, "Minimize Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Make the window full screen.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$fullscreen_window()
    #'
    #' session$close()
    #' }
    fullscreen_window = function() {
      req <- req_command(private$req, "Fullscreen Window", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      resp <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(resp)$value
    },
    #' @description
    #' Get the currently active element.
    #'
    #' @return A [WebElement] object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$get_active_element()
    #'
    #' session$close()
    #' }
    get_active_element = function() {
      req <- req_command(private$req, "Get Active Element", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value
      self$create_webelement(id[[1]])
    },
    #' @description
    #' Find the first element matching a selector.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return A [WebElement] object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "#download")
    #'
    #' session$find_element(using = "xpath", value = "//div[contains(@class, 'col-xs')]/h1")
    #'
    #' session$close()
    #' }
    find_element = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                            value, request_body = NULL) {
      using <- rlang::arg_match(using)
      check_string(value)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Find Element", session_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value), request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      id <- httr2::resp_body_json(response)$value
      self$create_webelement(id[[1]])
    },
    #' @description
    #' Find all elements matching a selector.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return A list of [WebElement] objects.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_elements(using = "css selector", value = "h1")
    #'
    #' session$find_elements(using = "xpath", value = "//h1")
    #'
    #' session$close()
    #' }
    find_elements = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                             value, request_body = NULL) {
      using <- rlang::arg_match(using)
      check_string(value)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Find Elements", session_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value), request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      ids <- httr2::resp_body_json(response)$value
      lapply(ids, function(x) self$create_webelement(x[[1]]))
    },
    #' @description
    #' Get the HTML source of the current page, serialized as a string.
    #'
    #' @return A string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$get_page_source()
    #'
    #' session$close()
    #' }
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
    #' accessed in the script using the `arguments` array. Can be [WebElement]
    #' objects or lists of such objects, which will be converted to nodes.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The return value of the script. Nodes or lists of nodes will
    #'   be converted to [WebElement] objects.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$execute_script("return 1")
    #'
    #' session$execute_script("return arguments[0] + arguments[1]", 1, 2)
    #'
    #' element <- session$find_element(value = "*")
    #'
    #' session$execute_script("return arguments[0]", element)
    #'
    #' session$close()
    #' }
    execute_script = function(x, ..., request_body = NULL) {
      check_string(x)
      check_dots_unnamed()
      args <- rlang::list2(...)
      check_list(request_body, allow_null = TRUE)

      args <- prepare_for_json(args)
      req <- req_command(private$req, "Execute Script", session_id = self$id)
      req <- req_body_selenium(req, list(script = x, args = args), request_body = request_body)
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
    #' [WebElement] objects or lists of such objects, which will be converted
    #' to nodes.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The return value of the script. Nodes or lists of nodes will
    #'   be converted to [WebElement] objects.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$execute_async_script("
    #'   let callback = arguments[arguments.length - 1];
    #'   callback(1)
    #' ")
    #'
    #' session$close()
    #' }
    execute_async_script = function(x, ..., request_body = NULL) {
      check_string(x)
      check_dots_unnamed()
      args <- rlang::list2(...)
      check_list(request_body, allow_null = TRUE)

      args <- prepare_for_json(args)
      req <- req_command(private$req, "Execute Async Script", session_id = self$id)
      req <- req_body_selenium(req, list(script = x, args = args), request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      parse_json_result(httr2::resp_body_json(response)$value, self)
    },
    #' @description
    #' Get all cookies.
    #'
    #' @return A list of cookies. Each cookie is a list with a `name` and
    #'   `value` field, along with some other optional fields.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$get_cookies()
    #'
    #' session$close()
    #' }
    get_cookies = function() {
      req <- req_command(private$req, "Get All Cookies", session_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get a specific cookie using its name.
    #'
    #' @param name The name of the cookie.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The cookie object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$add_cookie(list(name = "foo", value = "bar"))
    #'
    #' session$get_cookie("foo")
    #'
    #' session$close()
    #' }
    get_cookie = function(name, request_body = NULL) {
      check_string(name)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Get Named Cookie", session_id = self$id, name = name)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Add a cookie to the cookie store of the current document.
    #'
    #' @param cookie The cookie object to add: a list which must contain a
    #'   `name` and `value` field.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$add_cookie(list(name = "my_cookie", value = "1"))
    #'
    #' session$close()
    #' }
    add_cookie = function(cookie, request_body = NULL) {
      check_list(cookie)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Add Cookie", session_id = self$id)
      req <- req_body_selenium(req, list(cookie = cookie), request_body = request_body)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Delete a cookie using its name.
    #'
    #' @param name The name of the cookie.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$add_cookie(list(name = "foo", value = "bar"))
    #'
    #' session$delete_cookie("foo")
    #'
    #' session$close()
    #' }
    delete_cookie = function(name, request_body = NULL) {
      check_string(name)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Delete Cookie", session_id = self$id, name = name)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Delete all cookies in the cookie store of the current document.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$delete_all_cookies()
    #'
    #' session$close()
    #' }
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
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' actions <- actions_stream(
    #'   actions_press(keys$enter),
    #'   actions_pause(0.5),
    #'   actions_release(keys$enter)
    #' )
    #'
    #' session$perform_actions(actions)
    #'
    #' session$close()
    #' }
    perform_actions = function(actions, release_actions = TRUE, request_body = NULL) {
      check_class(actions, "selenium_actions_stream")
      check_bool(release_actions)
      check_list(request_body, allow_null = TRUE)

      actions <- unclass_stream(actions)
      req <- req_command(private$req, "Perform Actions", session_id = self$id)
      req <- req_body_selenium(req, list(actions = actions), request_body = request_body)
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
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' actions <- actions_stream(
    #'   actions_press("a")
    #' )
    #'
    #' session$perform_actions(actions, release_actions = FALSE)
    #'
    #' session$release_actions()
    #'
    #' session$close()
    #' }
    release_actions = function() {
      req <- req_command(private$req, "Release Actions", session_id = self$id)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Dismiss the current alert, if present.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$execute_script("alert('hello')")
    #'
    #' session$dismiss_alert()
    #'
    #' session$close()
    #' }
    dismiss_alert = function() {
      req <- req_command(private$req, "Dismiss Alert", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Accept the current alert, if present.
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$execute_script("alert('hello')")
    #'
    #' session$accept_alert()
    #'
    #' session$close()
    #' }
    accept_alert = function() {
      req <- req_command(private$req, "Accept Alert", session_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Get the message of the current alert, if present.
    #'
    #' @return The message of the current alert (a string).
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$execute_script("alert('hello')")
    #'
    #' session$get_alert_text()
    #'
    #' session$close()
    #' }
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
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The session object, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$execute_script("prompt('Enter text:')")
    #'
    #' session$send_alert_text("hello")
    #'
    #' session$close()
    #' }
    send_alert_text = function(text, request_body = NULL) {
      check_string(text)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Send Alert Text", session_id = self$id)
      req <- req_body_selenium(req, list(text = text), request_body = request_body)
      req_perform_selenium(req, verbose = private$verbose)
      invisible(self)
    },
    #' @description
    #' Take a screenshot of the current page.
    #'
    #' @return The base64-encoded PNG screenshot, as a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$screenshot()
    #'
    #' session$close()
    #' }
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
    #' @param scale The page scale, a number between 0.1 and 2.
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
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #'
    #' @return The base64-encoded PDF, as a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$print_page()
    #'
    #' session$close()
    #' }
    print_page = function(orientation = c("portrait", "landscape"),
                          scale = 1,
                          background = FALSE,
                          width = NULL,
                          height = NULL,
                          margin = NULL,
                          footer = NULL,
                          header = NULL,
                          shrink_to_fit = NULL,
                          page_ranges = NULL,
                          request_body = NULL) {
      orientation <- rlang::arg_match(orientation)
      check_number_decimal(scale, min = 0.1, max = 2)
      check_bool(background)
      check_number_decimal(width, min = 0, allow_null = TRUE)
      check_number_decimal(height, min = 0, allow_null = TRUE)
      if (is.list(margin)) {
        check_number_decimal(margin$left, allow_null = TRUE, min = 0)
        check_number_decimal(margin$right, allow_null = TRUE, min = 0)
        check_number_decimal(margin$top, allow_null = TRUE, min = 0)
        check_number_decimal(margin$bottom, allow_null = TRUE, min = 0)
        if (any(!names(margin) %in% c("left", "right", "top", "bottom"))) {
          bad_name <- names(margin)[!names(margin) %in% c("left", "right", "top", "bottom")][1]
          rlang::abort(c(
            "Argument 'margin' must be a list of four numbers, with names:",
            "'left', 'right', 'top', and 'bottom'.",
            "i" = paste0("Incorrect name: ", bad_name)
          ))
        }
      } else {
        check_number_decimal(margin, allow_null = TRUE, min = 0)
      }
      check_bool(shrink_to_fit, allow_null = TRUE)
      check_list(page_ranges, allow_null = TRUE)
      check_list(request_body, allow_null = TRUE)

      req <- req_command(private$req, "Print Page", session_id = self$id)

      if (!is.list(margin) && is.numeric(margin)) {
        margin <- list(
          left = margin,
          right = margin,
          top = margin,
          bottom = margin
        )
      }

      page <- compact(list(
        width = width,
        height = height
      ))

      page <- if (length(page) == 0) NULL else page

      body <- compact(list(
        orientation = rlang::arg_match(orientation),
        scale = scale,
        background = background,
        page = page,
        margin = margin,
        footer = footer,
        header = header,
        shrinkToFit = shrink_to_fit,
        pageRanges = page_ranges
      ))

      req <- req_body_selenium(req, body, request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose)
      httr2::resp_body_json(response)$value
    }
  ),
  private = list(
    req = NULL,
    verbose = NULL
  )
)
