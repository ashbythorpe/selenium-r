#' Create a set of actions to be performed
#'
#' `actions_stream()` creates a set of actions to be performed by
#' `SeleniumSession$perform_actions()`. Actions are a low level way to interact
#' with a page.
#'
#' @param ... `selenium_action` objects: the actions to perform.
#'
#' @returns A `selenium_actions_stream` object, ready to be passed into
#'   `SeleniumSession$perform_actions()`.
#'
#' @seealso
#' * Pause actions: [actions_pause()].
#' * Press actions: [actions_press()] and [actions_release()].
#' * Mouse actions: [actions_mousedown()], [actions_mouseup()]
#'   and [actions_mousemove()].
#' * Scroll actions: [actions_scroll()].
#'
#' @examples
#' actions_stream(
#'   actions_press(keys$enter),
#'   actions_pause(0.5),
#'   actions_release(keys$enter),
#'   actions_scroll(x = 1, y = 1, delta_x = 1, delta_y = 1, duration = 0.5),
#'   actions_mousemove(x = 1, y = 1, duration = 1, origin = "pointer")
#' )
#'
#' @export
actions_stream <- function(...) {
  actions <- rlang::list2(...)
  actions_stream <- list()
  for (action in actions) {
    check_class(action, "selenium_action", arg = I("All items in `...`"))
    actions_stream <- append_action(actions_stream, action)
  }

  class(actions_stream) <- "selenium_actions_stream"
  actions_stream
}

append_action <- function(x, action) {
  stopifnot(inherits(action, "selenium_action"))
  current_sequence <- if (length(x) == 0) NULL else x[[length(x)]]
  sequence_type <- current_sequence$type
  current_type <- sequence_type(action)

  if (!is.null(sequence_type)) {
    sequence <- add_to_sequence(current_sequence, sequence_type, action, current_type)
    if (!is.null(sequence)) {
      x[[length(x)]] <- sequence
      return(x)
    }
  }

  new_sequence <- new_actions_sequence(action)
  append(x, list(new_sequence))
}

add_to_sequence <- function(current_sequence, sequence_type, action, current_type) {
  if (sequence_type == current_type || current_type == "none") {
    current_sequence$actions <- append(current_sequence$actions, list(action))
  } else if (sequence_type == "none") {
    current_sequence$actions <- append(current_sequence$actions, list(action))
    current_sequence$type <- current_type
  } else {
    return(NULL)
  }

  current_sequence
}

new_actions_sequence <- function(first_action) {
  type <- sequence_type(first_action)
  list(type = type, id = rand_id(), actions = list(first_action))
}

sequence_type <- function(action) {
  if (inherits(action, "selenium_action_pause")) {
    "none"
  } else if (inherits(action, "selenium_action_key")) {
    "key"
  } else if (inherits(action, "selenium_action_pointer")) {
    "pointer"
  } else {
    "wheel"
  }
}

#' Wait for a period of time
#'
#' A pause action to be passed into [actions_stream()]. Waits for a given
#' number of seconds before performing the next action in the stream.
#'
#' @param seconds The number of seconds to wait for.
#'
#' @returns A `selenium_action` object.
#'
#' @examples
#' actions_stream(
#'   actions_pause(1)
#' )
#'
#' @export
actions_pause <- function(seconds) {
  check_number_decimal(seconds)

  data <- list(
    type = "pause",
    duration = seconds * 1000L
  )

  class(data) <- c("selenium_action", "selenium_action_pause")
  data
}

#' Press or release a key
#'
#' Key actions to be passed into [actions_stream()]. `actions_press()`
#' represents pressing a key on the keyboard, while `actions_release()`
#' represents releasing a key.
#'
#' @param key The key to press: a string consisting of a single character. Use
#'   the [keys] object to use special keys (e.g. <Ctrl>).
#'
#' @returns A `selenium_action` object.
#'
#' @examples
#' actions_stream(
#'   actions_press("a"),
#'   actions_release("a"),
#'   actions_press(keys$enter),
#'   actions_release(keys$enter)
#' )
#'
#' @export
actions_press <- function(key) {
  check_char(key)

  data <- list(
    type = "keyDown",
    value = key
  )

  class(data) <- c("selenium_action", "selenium_action_key", "selenium_action_press")
  data
}

#' @rdname actions_press
#'
#' @export
actions_release <- function(key) {
  check_char(key)

  data <- list(
    type = "keyUp",
    value = key
  )

  class(data) <- c("selenium_action", "selenium_action_key", "selenium_action_release")
  data
}

#' Press, release or move the mouse.
#'
#' Mouse actions to be passed into [actions_stream()]. `actions_mousedown()`
#' represents pressing a button on the mouse, while `actions_mouseup()`
#' represents releasing a button. `actions_mousemove()` represents moving the
#' mouse.
#'
#' @param button The mouse button to press.
#' @param width The 'width' of the click, a number.
#' @param height The 'height' of the click, a number.
#' @param pressure The amount of pressure to apply to the click: a number
#'   between 0 and 1.
#' @param tangential_pressure A number between 0 and 1.
#' @param tilt_x A whole number between -90 and 90.
#' @param tilt_y A whole number between -90 and 90.
#' @param twist A whole number between 0 and 359.
#' @param altitude_angle A number between 0 and `pi/2`.
#' @param azimuth_angle A number between 0 and `2*pi`.
#'
#' @returns A `selenium_action` object.
#'
#' @examples
#' actions_stream(
#'   actions_mousedown("left", width = 1, height = 1, pressure = 0.5),
#'   actions_mouseup("left", width = 100, height = 50, pressure = 1),
#'   actions_mousemove(x = 1, y = 1, duration = 1, origin = "pointer")
#' )
#'
#' @export
actions_mousedown <- function(button = c("left", "right", "middle"),
                              width = NULL,
                              height = NULL,
                              pressure = NULL,
                              tangential_pressure = NULL,
                              tilt_x = NULL,
                              tilt_y = NULL,
                              twist = NULL,
                              altitude_angle = NULL,
                              azimuth_angle = NULL) {
  button <- rlang::arg_match(button)
  button <- switch(button,
    "left" = 0,
    "middle" = 1,
    "right" = 2
  )
  check_number_whole(width, min = 0, allow_null = TRUE)
  check_number_whole(height, min = 0, allow_null = TRUE)
  check_number_decimal(pressure, min = 0, max = 1, allow_null = TRUE)
  check_number_decimal(tangential_pressure, min = 0, max = 1, allow_null = TRUE)
  check_number_whole(tilt_x, min = -90, max = 90, allow_null = TRUE)
  check_number_whole(tilt_y, min = -90, max = 90, allow_null = TRUE)
  check_number_whole(twist, min = 0, max = 359, allow_null = TRUE)
  check_number_decimal(altitude_angle, min = 0, max = pi / 2, allow_null = TRUE)
  check_number_decimal(azimuth_angle, min = 0, max = 2 * pi, allow_null = TRUE)

  parameters <- compact(list(
    button = button,
    width = width,
    height = height,
    pressure = pressure,
    tangentialPressure = tangential_pressure,
    tiltX = tilt_x,
    tiltY = tilt_y,
    twist = twist,
    altitudeAngle = altitude_angle,
    azimuthAngle = azimuth_angle
  ))

  data <- rlang::list2(
    type = "pointerDown",
    !!!parameters
  )

  class(data) <- c("selenium_action", "selenium_action_pointer", "selenium_action_pointer_down")
  data
}

#' @rdname actions_mousedown
#'
#' @export
actions_mouseup <- function(button = c("left", "right", "middle"),
                            width = NULL,
                            height = NULL,
                            pressure = NULL,
                            tangential_pressure = NULL,
                            tilt_x = NULL,
                            tilt_y = NULL,
                            twist = NULL,
                            altitude_angle = NULL,
                            azimuth_angle = NULL) {
  button <- rlang::arg_match(button)
  button <- switch(button,
    "left" = 0,
    "middle" = 1,
    "right" = 2
  )
  check_number_whole(width, min = 0, allow_null = TRUE)
  check_number_whole(height, min = 0, allow_null = TRUE)
  check_number_decimal(pressure, min = 0, max = 1, allow_null = TRUE)
  check_number_decimal(tangential_pressure, min = 0, max = 1, allow_null = TRUE)
  check_number_whole(tilt_x, min = -90, max = 90, allow_null = TRUE)
  check_number_whole(tilt_y, min = -90, max = 90, allow_null = TRUE)
  check_number_whole(twist, min = 0, max = 359, allow_null = TRUE)
  check_number_decimal(altitude_angle, min = 0, max = pi / 2, allow_null = TRUE)
  check_number_decimal(azimuth_angle, min = 0, max = 2 * pi, allow_null = TRUE)

  parameters <- compact(list(
    button = button,
    width = width,
    height = height,
    pressure = pressure,
    tangentialPressure = tangential_pressure,
    tiltX = tilt_x,
    tiltY = tilt_y,
    twist = twist,
    altitudeAngle = altitude_angle,
    azimuthAngle = azimuth_angle
  ))

  data <- rlang::list2(
    type = "pointerUp",
    !!!parameters
  )

  class(data) <- c("selenium_action", "selenium_action_pointer", "selenium_action_pointer_up")
  data
}

#' @rdname actions_mousedown
#'
#' @param x The x coordinate of the mouse movement.
#' @param y The y coordinate of the mouse movement.
#' @param duration The duration of the mouse movement, in seconds.
#' @param origin The point from which `x` and `y` are measured. Can be a
#'   `WebElement` object, in which case `x` and `y` are measured from the
#'   center of the element.
#'
#' @export
actions_mousemove <- function(x,
                              y,
                              duration = NULL,
                              origin = c("viewport", "pointer"),
                              width = NULL,
                              height = NULL,
                              pressure = NULL,
                              tangential_pressure = NULL,
                              tilt_x = NULL,
                              tilt_y = NULL,
                              twist = NULL,
                              altitude_angle = NULL,
                              azimuth_angle = NULL) {
  check_number_whole(x)
  check_number_whole(y)
  check_number_decimal(duration, min = 0, allow_null = TRUE)
  if (inherits(origin, "WebElement")) {
    origin <- origin$toJSON()
  } else {
    origin <- rlang::arg_match(origin)
  }
  check_number_whole(width, min = 0, allow_null = TRUE)
  check_number_whole(height, min = 0, allow_null = TRUE)
  check_number_decimal(pressure, min = 0, max = 1, allow_null = TRUE)
  check_number_decimal(tangential_pressure, min = 0, max = 1, allow_null = TRUE)
  check_number_whole(tilt_x, min = -90, max = 90, allow_null = TRUE)
  check_number_whole(tilt_y, min = -90, max = 90, allow_null = TRUE)
  check_number_whole(twist, min = 0, max = 359, allow_null = TRUE)
  check_number_decimal(altitude_angle, min = 0, max = pi / 2, allow_null = TRUE)
  check_number_decimal(azimuth_angle, min = 0, max = 2 * pi, allow_null = TRUE)

  parameters <- compact(list(
    x = x,
    y = y,
    duration = duration * 1000L,
    origin = origin,
    width = width,
    height = height,
    pressure = pressure,
    tangentialPressure = tangential_pressure,
    tiltX = tilt_x,
    tiltY = tilt_y,
    twist = twist,
    altitudeAngle = altitude_angle,
    azimuthAngle = azimuth_angle
  ))

  data <- rlang::list2(
    type = "pointerMove",
    !!!parameters
  )

  class(data) <- c("selenium_action", "selenium_action_pointer", "selenium_action_pointer_move")
  data
}

#' Scroll the page
#'
#' Scroll actions to be passed into [actions_stream()]. Scroll the page in
#' a given direction.
#'
#' @param x The x coordinate from which the scroll action originates from.
#' @param y The y coordinate from which the scroll action originates from.
#' @param delta_x The number of pixels to scroll in the x direction.
#' @param delta_y The number of pixels to scroll in the y direction.
#' @param duration The duration of the scroll, in seconds.
#' @param origin The point from which `x` and `y` are measured. Can be a
#'   `WebElement` object, in which case `x` and `y` are measured from the
#'   center of the element. Otherwise, `origin` must be `"viewport"`.
#'
#' @returns A `selenium_action` object.
#'
#' @examples
#' actions_stream(
#'   actions_scroll(x = 1, y = 1, delta_x = 1, delta_y = 1, duration = 0.5)
#' )
#'
#' @export
actions_scroll <- function(x,
                           y,
                           delta_x,
                           delta_y,
                           duration = NULL,
                           origin = "viewport") {
  check_number_whole(x)
  check_number_whole(y)
  check_number_whole(delta_x)
  check_number_whole(delta_y)
  check_number_decimal(duration, min = 0, allow_null = TRUE)
  if (inherits(origin, "WebElement")) {
    origin <- origin$toJSON()
  } else {
    origin <- rlang::arg_match(origin)
  }

  parameters <- compact(list(
    x = x,
    y = y,
    duration = duration * 1000L,
    origin = origin,
    deltaX = delta_x,
    deltaY = delta_y
  ))

  data <- rlang::list2(
    type = "scroll",
    !!!parameters
  )

  class(data) <- c("selenium_action", "selenium_action_scroll")
  data
}

unclass_stream <- function(x) {
  lapply(x, unclass_actions)
}

unclass_actions <- function(x) {
  x$actions <- lapply(x$actions, unclass)
  x
}
