test_that("Opening and closing a SeleniumSession works", {
  session <- test_session()

  expect_type(session$id, "character")

  session$close()
})

test_that("Server status works", {
  session <- test_session()

  expect_true(session$status()$ready, TRUE)

  session$close()

  expect_true(session$status()$ready, TRUE)
})

test_that("Getting and setting timeouts works", {
  session <- test_session()

  expect_type(session$get_timeouts(), "list")
  expect_length(session$get_timeouts(), 3)

  session$set_timeouts(script = 100)
  expect_equal(session$get_timeouts()$script, 100)

  session$set_timeouts(page_load = 200, implicit_wait = 300)
  timeouts <- session$get_timeouts()
  expect_equal(timeouts$script, 100)
  expect_equal(timeouts$pageLoad, 200)
  expect_equal(timeouts$implicit, 300)

  session$close()
})

test_that("Navigating works", {
  session <- test_session()

  session$navigate("https://www.google.com/")

  expect_equal(session$current_url(), "https://www.google.com/")

  session$navigate("https://www.r-project.org/")

  expect_equal(session$current_url(), "https://www.r-project.org/")

  session$back()

  expect_equal(session$current_url(), "https://www.google.com/")

  session$forward()

  expect_equal(session$current_url(), "https://www.r-project.org/")

  session$refresh()

  expect_equal(session$current_url(), "https://www.r-project.org/")

  session$navigate("https://www.tidyverse.org/")

  expect_equal(session$title(), "Tidyverse")

  session$close()
})

test_that("Windows work", {
  session <- test_session()

  session$navigate("https://www.r-project.org/")

  result <- session$new_window()

  expect_equal(result$type, "tab")
  expect_type(result$handle, "character")

  expect_length(session$window_handles(), 2)
  expect_equal(session$window_handles()[[2]], result$handle)

  expect_equal(session$current_url(), "https://www.r-project.org/")

  session$switch_to_window(result$handle)

  handles <- session$close_window()
  expect_equal(session$window_handles(), handles)
  session$switch_to_window(handles[[1]])

  expect_equal(session$current_url(), "https://www.r-project.org/")

  result <- session$new_window(type = "window")

  expect_equal(result$type, "window")
  expect_type(result$handle, "character")

  session$switch_to_window(result$handle)

  session$close()
})

test_that("Switching to frames works", {
  session <- test_session()

  session$navigate("https://www.youtube.com")

  expect_true(length(session$find_elements(value = "iframe")) > 0)

  session$switch_to_frame(0)

  expect_error(session$switch_to_frame(0))

  session$switch_to_frame()

  element <- session$find_element(value = "iframe")

  session$switch_to_frame(element)

  session$switch_to_parent_frame()

  session$switch_to_frame(0)

  session$switch_to_frame(NA)

  session$close()
})

test_that("Changing window size works", {
  session <- test_session()

  session$navigate("https://www.r-project.org")

  expect_length(session$get_window_rect(), 4)

  rect <- session$set_window_rect(width = 800, height = 600, x = 2, y = 3)

  expect_equal(session$get_window_rect(), rect)

  other_window <- session$new_window(type = "window")$handle

  session$switch_to_window(other_window)

  # These can fail on some browsers, but at least take a look at the output
  try(session$maximize_window())
  try(session$minimize_window())
  try(session$fullscreen_window())

  session$close()
})

test_that("Finding elements works", {
  session <- test_session()

  expect_s3_class(session$get_active_element(), "WebElement")

  expect_equal(session$find_element(value = "*")$get_tag_name(), "html")

  elements <- session$find_elements(value = "*")
  expect_length(elements, 3)

  for (element in elements) {
    expect_s3_class(element, "WebElement")
  }

  session$close()
})

test_that("Get page source works", {
  session <- test_session()

  source <- session$get_page_source()

  expect_type(source, "character")

  skip_if_not_installed("xml2")

  expect_no_error(xml2::read_html(source))

  session$navigate("https://www.r-project.org/")

  source <- session$get_page_source()

  expect_no_error(xml2::read_html(source))

  session$close()
})

test_that("Execute script works", {
  session <- test_session()

  element <- session$execute_script("return document.querySelector('*');")
  expect_s3_class(element, "WebElement")

  element2 <- session$find_element(value = "*")
  expect_equal(element$id, element2$id)

  element3 <- session$execute_script("return arguments[0];", element)
  expect_equal(element$id, element3$id)

  expect_equal(session$execute_script("return arguments[0] + arguments[1];", 1, 1), 2)

  x <- session$execute_async_script("
    let callback = arguments[arguments.length - 1];
    let check = function(n) {
      if (n == 0) {
        return callback(n);
      } else {
        setTimeout(function() {
          check(n - 1);
        }, 1000);
      }
    }
    check(2);
  ")

  expect_equal(x, 0)

  session$close()
})

test_that("Changing cookies works", {
  session <- test_session()

  session$navigate("https://www.google.com/")

  cookies <- session$get_cookies()

  expect_true(length(cookies) > 0)

  cookie_1 <- cookies[[1]]

  expect_equal(session$get_cookie(cookie_1$name), cookie_1)

  my_cookie <- list(name = "my_cookie", value = "my_value")

  session$add_cookie(my_cookie)

  expect_equal(session$get_cookie("my_cookie")$value, "my_value")

  session$delete_cookie("my_cookie")

  expect_error(session$get_cookie("my_cookie"))

  session$delete_all_cookies()

  expect_length(session$get_cookies(), 0)

  session$close()
})

test_that("Performing actions works", {
  session <- test_session()

  session$navigate("https://www.google.com/")

  element <- session$find_element(value = "*")

  actions <- actions_stream(
    actions_pause(1),
    actions_press("a"),
    actions_release("a"),
    actions_mousedown(
      button = "middle", width = 1, height = 1, pressure = 0.5,
      tangential_pressure = 1, tilt_x = 1, tilt_y = 1,
      twist = 2, altitude_angle = 1, azimuth_angle = 2
    ),
    actions_mouseup(
      button = "middle", width = 100, height = 50, pressure = 1,
      tangential_pressure = 0.1, tilt_x = -1, tilt_y = 8,
      twist = 10, altitude_angle = pi / 2 - 1, azimuth_angle = 0
    ),
    actions_mousemove(x = 1, y = 1, duration = 1, origin = "pointer"),
    actions_scroll(x = 1, y = 1, delta_x = 1, delta_y = 1, duration = 0.5),
    actions_mousemove(x = 0, y = 0, origin = element),
    actions_scroll(x = 0, y = 0, delta_x = 1, delta_y = 1, origin = element)
  )

  expect_no_error(session$perform_actions(actions))

  session$close()
})

test_that("Handling alerts works", {
  session <- test_session()

  session$execute_script("alert('Hello')")
  expect_equal(session$get_alert_text(), "Hello")
  session$dismiss_alert()

  session$execute_script("alert('Hello')")
  session$accept_alert()

  session$execute_script("prompt('Enter text:')")
  session$send_alert_text("Hello")
  session$dismiss_alert()

  session$close()
})

test_that("Screenshotting and printing works", {
  session <- test_session()

  expect_no_error(session$screenshot())

  # Doesn't work on all browsers
  try(session$print_page())

  session$close()
})
