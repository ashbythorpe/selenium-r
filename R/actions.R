actions_stream <- function(...) {
  actions <- rlang::list2(...)
  actions_stream <- list()
  for (action in actions) {
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

actions_pause <- function(x) {
  data <- list(
    type = "pause",
    duration = x * 1000L
  )

  class(data) <- c("selenium_action", "selenium_action_pause")
  data
}

actions_press <- function(key) {
  data <- list(
    type = "keyDown",
    value = key
  )

  class(data) <- c("selenium_action", "selenium_action_key", "selenium_action_press")
  data
}

actions_release <- function(key) {
  data <- list(
    type = "keyUp",
    value = key
  )

  class(data) <- c("selenium_action", "selenium_action_key", "selenium_action_release")
  data
}

actions_mousedown <- function(button = 0,
                              width = NULL,
                              height = NULL,
                              pressure = NULL,
                              tangential_pressure = NULL,
                              tilt_x = NULL,
                              tilt_y = NULL,
                              twist = NULL,
                              altitude_angle = NULL,
                              azimuth_angle = NULL) {
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

actions_mouseup <- function(button = 0,
                            width = NULL,
                            height = NULL,
                            pressure = NULL,
                            tangential_pressure = NULL,
                            tilt_x = NULL,
                            tilt_y = NULL,
                            twist = NULL,
                            altitude_angle = NULL,
                            azimuth_angle = NULL) {
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

actions_mousemove <- function(x,
                              y,
                              duration = NULL,
                              origin = c("viewport", "pointer")) {
  if (inherits(origin, "WebElement")) {
    origin <- origin$toJSON()
  } else {
    origin <- rlang::arg_match(origin)
  }

  parameters <- compact(list(
    x = x,
    y = y,
    duration = duration,
    origin = origin
  ))

  data <- rlang::list2(
    type = "pointerMove",
    !!!parameters
  )

  class(data) <- c("selenium_action", "selenium_action_pointer", "selenium_action_pointer_move")
  data
}

actions_scroll <- function(x,
                           y,
                           delta_x,
                           delta_y,
                           duration = NULL,
                           origin = "viewport") {
  if (inherits(origin, "WebElement")) {
    origin <- origin$toJSON()
  }

  parameters <- compact(list(
    x = x,
    y = y,
    duration = duration,
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
