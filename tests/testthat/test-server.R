test_that("selenium_server() works", {
  skip_if_offline()
  skip_if(is_check())
  skip_on_ci()

  dir <- withr::local_tempdir()
  withr::local_envvar(list(R_USER_DATA_DIR = dir))

  expect_no_error({
    session <- selenium_server(interactive = FALSE)

    session$kill()

    session2 <- selenium_server(interactive = FALSE, version = "4.10.0")

    session2$kill()
  })
})
