test_that("Finding WebElements works", {
  session <- test_session()

  element <- session$find_element(value = "*")

  expect_s3_class(element, "WebElement")

  element_2 <- element$find_element(value = "*")

  expect_s3_class(element_2, "WebElement")

  elements <- element$find_elements(value = "*")

  expect_equal(element_2$id, elements[[1]]$id)

  session$close()
})

test_that("Predicates on WebElements work", {
  session <- test_helper_site()

  element <- session$find_element(value = "#radio-buttons")$find_element(value = "#radio-1")

  expect_equal(element$is_selected(), FALSE)
  element$click()
  expect_equal(element$is_selected(), TRUE)

  buttons <- session$find_element(value = "#buttons")$find_elements(value = "button")
  expect_equal(buttons[[1]]$is_enabled(), TRUE)
  expect_equal(buttons[[2]]$is_enabled(), FALSE)

  expect_equal(buttons[[1]]$is_displayed(), TRUE)

  session$close()
})

test_that("Properties work", {
  session <- test_helper_site()

  div <- session$find_element(value = "#properties")

  button <- div$find_element(value = "button")
  expect_equal(button$get_attribute("id"), "property-1")
  input <- div$find_element(value = "input")
  input$send_keys(" A")
  expect_equal(input$get_property("value"), "Initial value A")
  expect_equal(input$get_attribute("value"), "Initial value")

  hidden_div <- div$find_element(value = ".hidden-div")
  expect_equal(hidden_div$get_css_value("display"), "none")

  p <- div$find_element(value = ".text")
  expect_equal(p$get_text(), "Example text")
  expect_equal(div$get_tag_name(), "div")
  expect_equal(p$get_tag_name(), "p")

  fixed_size_div <- div$find_element(value = ".fixed-size")
  expect_equal(fixed_size_div$get_rect()[c("width", "height")], list(width = 100, height = 100))

  radio_button <- session$find_element(value = "#radio-buttons")$find_element(value = "#radio-1")

  expect_equal(fixed_size_div$computed_role(), "generic")
  expect_equal(radio_button$computed_label(), "Option 1")

  expect_no_error(radio_button$screenshot())

  session$close()
})

test_that("Actions work", {
  session <- test_helper_site()

  div <- session$find_element(value = "#interactible")

  p <- div$find_element(value = "p")

  expect_true(p$get_css_value("display") != "none")

  button <- div$find_element(value = "button")
  button$click()
  Sys.sleep(0.1)
  expect_equal(p$get_css_value("display"), "none")

  input <- div$find_element(value = "input")

  expect_equal(input$get_property("value"), "Initial value")
  input$clear()
  expect_equal(input$get_property("value"), "")
  input$send_keys("A")
  expect_equal(input$get_property("value"), "A")

  session$close()
})

test_that("ShadowRoot elements work", {
  session <- test_helper_site(verbose = TRUE)

  div <- session$find_element(value = "#shadow-container")

  shadow_root <- div$shadow_root()

  expect_s3_class(shadow_root, "ShadowRoot")

  expect_length(shadow_root$find_elements(value = "*"), 2)
  expect_equal(shadow_root$find_element(value = "p")$get_text(), "Me too!")

  res <- session$execute_script("
    const root = arguments[0];
    const div = document.createElement('div');
    div.innerText = 'Me three!';
    root.appendChild(div);
    return root;
  ", shadow_root)

  expect_equal(res$id, shadow_root$id)

  expect_length(shadow_root$find_elements(value = "*"), 3)

  session$close()
})
