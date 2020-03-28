test_that("corrupt tsibble object", {
  attr(pedestrian, "regular") <- TRUE
  expect_warning(format(pedestrian), "corrupt tsibble")
  attr(tourism, "ordered") <- TRUE
  expect_warning(format(tourism), "corrupt tsibble")
})
