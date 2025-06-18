#' Create a live element
#'
#' @description
#' This class represents a single element on the page. It is created using an
#' existing [SeleniumSession] instance.
#'
#' @export
WebElement <- R6::R6Class("WebElement",
  public = list(
    #' @field id The id of the element, used to uniquely identify it on the
    #'   page.
    id = NULL,

    #' @description
    #' Initialize a `WebElement` object. This should not be called manually:
    #' instead use [SeleniumSession$create_webelement()][SeleniumSession] if
    #' you have an element id. To find elements on the page, use
    #' [SeleniumSession$find_element()][SeleniumSession] and
    #' [SeleniumSession$find_elements()][SeleniumSession].
    #'
    #' @param session_id The id of the session that the element belongs to.
    #' @param req,verbose Private fields of a [SeleniumSession] object.
    #' @param id The element id.
    #'
    #' @return A `WebElement` object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' element <- session$find_element(using = "css selector", value = "#download")
    #'
    #' session$close()
    #' }
    initialize = function(session_id, req, verbose, id) {
      private$session_id <- session_id
      private$req <- req
      private$verbose <- verbose
      self$id <- id
    },
    #' @description
    #' A shadow DOM is a self-contained DOM tree, contained within another DOM
    #' tree. A shadow root is an element that contains a DOM subtree. This
    #' method gets the shadow root property of an element.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A [ShadowRoot] object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' # Let's create our own Shadow Root using JavaScript
    #' session$execute_script("
    #'   const div = document.createElement('div');
    #'   document.body.appendChild(div);
    #'   div.attachShadow({mode: 'open'});
    #' ")
    #'
    #' element <- session$find_element(using = "css selector", value = "div")
    #'
    #' shadow_root <- element$shadow_root()
    #'
    #' session$close()
    #' }
    shadow_root = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Element Shadow Root", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      id <- httr2::resp_body_json(response)$value[[1]]
      ShadowRoot$new(private$session_id, private$req, private$verbose, id)
    },
    #' @description
    #' Find the first element matching a selector, relative to the current
    #' element.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A `WebElement` object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' row <- session$find_element(using = "css selector", value = ".row")
    #'
    #' logo_container <- row$find_element(using = "css selector", value = "p")
    #'
    #' logo <- logo_container$find_element(using = "css selector", value = "img")
    #'
    #' session$close()
    #' }
    find_element = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                            value,
                            request_body = NULL,
                            timeout = 20) {
      using <- rlang::arg_match(using)
      check_string(value)
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Find Element From Element", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value), request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      id <- httr2::resp_body_json(response)$value[[1]]
      WebElement$new(private$session_id, private$req, private$verbose, id)
    },
    #' @description
    #' Find all elements matching a selector, relative to the current element.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A list of `WebElement` objects.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' row <- session$find_element(using = "css selector", value = ".row")
    #'
    #' links <- row$find_elements(using = "css selector", value = "a")
    #'
    #' session$close()
    #' }
    find_elements = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                             value,
                             request_body = NULL, timeout = 20) {
      using <- rlang::arg_match(using)
      check_string(value)
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Find Elements From Element", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value), request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      ids <- httr2::resp_body_json(response)$value
      lapply(ids, function(id) WebElement$new(private$session_id, private$req, private$verbose, id[[1]]))
    },
    #' @description
    #' Check if an element is currently selected.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A boolean value: `TRUE` or `FALSE`.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "#download")$is_selected()
    #'
    #' session$close()
    #' }
    is_selected = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Is Element Selected", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get an attribute from an element.
    #'
    #' @param name The name of the attribute.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The value of the attribute: a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$get_attribute("href")
    #'
    #' session$close()
    #' }
    get_attribute = function(name, request_body = NULL, timeout = 20) {
      check_string(name)
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Element Attribute", session_id = private$session_id, element_id = self$id, name = name)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get a property from an element. Properties are similar to attributes,
    #' but represent the HTML source code of the page, rather than the current
    #' state of the DOM.
    #'
    #' @param name The name of the property.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The value of the property: a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$get_property("href")
    #'
    #' session$close()
    #' }
    get_property = function(name, request_body = NULL, timeout = 20) {
      check_string(name)
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Element Property", session_id = private$session_id, element_id = self$id, name = name)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the computed value of a CSS property.
    #'
    #' @param name The name of the CSS property.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The value of the CSS property: a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$get_css_value("color")
    #'
    #' session$close()
    #' }
    get_css_value = function(name, request_body = NULL, timeout = 20) {
      check_string(name)
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Element CSS Value", session_id = private$session_id, element_id = self$id, "property name" = name)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the text content of an element.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The text content of the element: a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "#download")$get_text()
    #'
    #' session$close()
    #' }
    get_text = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Element Text", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the tag name of an element.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The tag name of the element: a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "#download")$get_tag_name()
    #'
    #' session$close()
    #' }
    get_tag_name = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Element Tag Name", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the dimensions and coordinates of an element.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A list containing the following elements:
    #'
    #' * `x`: The x-coordinate of the element.
    #' * `y`: The y-coordinate of the element.
    #' * `width`: The width of the element in pixels.
    #' * `height`: The height of the element in pixels.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "#download")$get_rect()
    #'
    #' session$close()
    #' }
    get_rect = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Element Rect", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Check if an element is currently enabled.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A boolean value: `TRUE` or `FALSE`.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$is_enabled()
    #'
    #' session$close()
    #' }
    is_enabled = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Is Element Enabled", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the computed role of an element. The role of an element is usually
    #' "generic", but is often used when an elements tag name differs from its
    #' purpose. For example, a link that is "button-like" in nature may have
    #' a "button" role.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$computed_role()
    #'
    #' session$close()
    #' }
    computed_role = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Computed Role", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Get the computed label of an element (i.e. The text of the label element
    #' that points to the current element).
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$computed_label()
    #'
    #' session$close()
    #' }
    computed_label = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Get Computed Label", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Click on an element.
    #'
    #' @return The element, invisibly.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$click()
    #'
    #' session$close()
    #' }
    click = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Element Click", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      invisible(self)
    },
    #' @description
    #' Clear the contents of a text input element.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The element, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.google.com")
    #'
    #' session$find_element(using = "css selector", value = "textarea")$clear()
    #'
    #' session$close()
    #' }
    clear = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Element Clear", session_id = private$session_id, element_id = self$id)
      req <- req_body_selenium(req, NULL)
      req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      invisible(self)
    },
    #' @description
    #' Send keys to an element.
    #'
    #' @param ... The keys to send (strings). Use [keys] for special keys, and
    #'   use [key_chord()] to send keys combinations.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The element, invisibly.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.google.com")
    #'
    #' input <- session$find_element(using = "css selector", value = "textarea")
    #'
    #' input$send_keys("Hello")
    #'
    #' input$send_keys(key_chord(keys$control, "a"), key_chord(keys$control, "c"))
    #'
    #' input$send_keys(keys$control, "v")
    #'
    #' input$get_attribute("value")
    #'
    #' session$close()
    #' }
    send_keys = function(..., request_body = NULL, timeout = 20) {
      for (a in list2(...)) {
        check_string(a, arg = I("Every element in `...`"))
      }
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Element Send Keys", session_id = private$session_id, element_id = self$id)
      body <- list(
        text = paste0(...)
      )
      req <- req_body_selenium(req, body, request_body = request_body)
      req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      invisible(self)
    },
    #' @description
    #' Take a screenshot of an element.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return The base64-encoded PNG screenshot, as a string.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$screenshot()
    #'
    #' session$close()
    #' }
    screenshot = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Take Element Screenshot", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Check if an element is displayed. This function may not work on all
    #' platforms.
    #'
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A boolean.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' session$find_element(using = "css selector", value = "a")$is_displayed()
    #'
    #' session$close()
    #' }
    is_displayed = function(timeout = 20) {
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Element Displayed", session_id = private$session_id, element_id = self$id)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      httr2::resp_body_json(response)$value
    },
    #' @description
    #' Convert an element to JSON. This is used by
    #' [SeleniumSession$execute_script()][SeleniumSession].
    #'
    #' @return A list, which can then be converted to JSON using
    #' [jsonlite::toJSON()].
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' session$navigate("https://www.r-project.org")
    #'
    #' result <- session$find_element(using = "css selector", value = "a")$toJSON()
    #'
    #' result
    #'
    #' jsonlite::toJSON(result, auto_unbox = TRUE)
    #'
    #' session$close()
    #' }
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

#' Create a shadow root
#'
#' @description
#'
#' A shadow DOM is a self-contained DOM tree, contained within another DOM
#' tree. A shadow root is an element that contains a DOM subtree. This class
#' represents a shadow root object, allowing you to select elements within
#' the shadow root.
#'
#' @export
ShadowRoot <- R6::R6Class("ShadowRoot",
  public = list(
    #' @field id The id of the shadow root.
    id = NULL,

    #' @description
    #' Initialize a new `ShadowRoot` object. This should not be called
    #' manually: instead use [WebElement$shadow_root()][WebElement], or
    #' [SeleniumSession$create_shadow_root()][SeleniumSession].
    #'
    #' @param session_id The id of the session.
    #' @param req,verbose Private fields of a [SeleniumSession] object.
    #' @param id The id of the shadow root.
    #'
    #' @return A `ShadowRoot` object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' # Let's create our own Shadow Root using JavaScript
    #' session$execute_script("
    #'   const div = document.createElement('div');
    #'   document.body.appendChild(div);
    #'   div.attachShadow({mode: 'open'});
    #' ")
    #'
    #' element <- session$find_element(using = "css selector", value = "div")
    #'
    #' element$shadow_root()
    #'
    #' session$close()
    #' }
    initialize = function(session_id, req, verbose, id) {
      private$session_id <- session_id
      private$req <- req
      private$verbose <- verbose
      self$id <- id
    },
    #' @description
    #' Find an element in the shadow root.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A [WebElement] object.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' # Let's create our own Shadow Root using JavaScript
    #' session$execute_script("
    #'   const div = document.createElement('div');
    #'   document.body.appendChild(div);
    #'   const shadowRoot = div.attachShadow({mode: 'open'});
    #'   const span = document.createElement('span');
    #'   span.textContent = 'Hello';
    #'   shadowRoot.appendChild(span);
    #' ")
    #'
    #' element <- session$find_element(using = "css selector", value = "div")
    #'
    #' shadow_root <- element$shadow_root()
    #'
    #' shadow_root$find_element(using = "css selector", value = "span")
    #'
    #' session$close()
    #' }
    find_element = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                            value, request_body = NULL, timeout = 20) {
      using <- rlang::arg_match(using)
      check_string(value)
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      req <- req_command(private$req, "Find Element From Shadow Root", session_id = private$session_id, shadow_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value), request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      id <- httr2::resp_body_json(response)$value
      WebElement$new(private$session_id, private$req, private$verbose, id[[1]])
    },
    #' @description
    #' Find all elements in a shadow root matching a selector.
    #'
    #' @param using The type of selector to use.
    #' @param value The value of the selector: a string.
    #' @param request_body A list of request body parameters to pass to the
    #'   Selenium server, overriding the default body of the web request
    #' @param timeout How long to wait for a request to receive a response
    #'   before throwing an error.
    #'
    #' @return A list of [WebElement] objects.
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' # Let's create our own Shadow Root using JavaScript
    #' session$execute_script("
    #'   const div = document.createElement('div');
    #'   document.body.appendChild(div);
    #'   const shadowRoot = div.attachShadow({mode: 'open'});
    #'   const span = document.createElement('span');
    #'   span.textContent = 'Hello';
    #'   shadowRoot.appendChild(span);
    #'   const p = document.createElement('p');
    #'   p.textContent = 'Me too!';
    #'   shadowRoot.appendChild(p);
    #' ")
    #'
    #' element <- session$find_element(using = "css selector", value = "div")
    #'
    #' shadow_root <- element$shadow_root()
    #'
    #' shadow_root$find_elements(using = "css selector", value = "*")
    #'
    #' session$close()
    #' }
    find_elements = function(using = c("css selector", "xpath", "tag name", "link text", "partial link text"),
                             value, request_body = NULL, timeout = 20) {
      using <- rlang::arg_match(using)
      check_string(value)
      check_list(request_body, allow_null = TRUE)
      check_number_decimal(timeout, allow_null = TRUE)

      using <- rlang::arg_match(using)
      req <- req_command(private$req, "Find Elements From Shadow Root", session_id = private$session_id, shadow_id = self$id)
      req <- req_body_selenium(req, list(using = using, value = value), request_body = request_body)
      response <- req_perform_selenium(req, verbose = private$verbose, timeout = timeout)
      ids <- httr2::resp_body_json(response)$value
      lapply(ids, function(id) WebElement$new(private$session_id, private$req, private$verbose, id[[1]]))
    },
    #' @description
    #' Convert an element to JSON. This is used by
    #' [SeleniumSession$execute_script()][SeleniumSession].
    #'
    #' @return A list, which can then be converted to JSON using
    #' [jsonlite::toJSON()].
    #'
    #' @examples
    #' \dontrun{
    #' session <- SeleniumSession$new()
    #'
    #' # Let's create our own Shadow Root using JavaScript
    #' session$execute_script("
    #'   const div = document.createElement('div');
    #'   document.body.appendChild(div);
    #'   div.attachShadow({mode: 'open'});
    #' ")
    #'
    #' element <- session$find_element(using = "css selector", value = "div")
    #'
    #' shadow_root <- element$shadow_root()
    #'
    #' result <- shadow_root$toJSON()
    #'
    #' result
    #'
    #' jsonlite::toJSON(result, auto_unbox = TRUE)
    #'
    #' session$close()
    #' }
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
