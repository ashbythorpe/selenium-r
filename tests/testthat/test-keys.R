test_that("key_chord() works", {
  expect_equal(
    key_chord(keys$enter, keys$shift, "A"),
    paste0(keys$enter, keys$shift, "A", keys$null)
  )
})
