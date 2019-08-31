context("interval")

test_that("multiple arguments matching", {
  expect_error(
    new_interval(y = 1, m = 2, mi = 3, second = 4),
    "Invalid argument:"
  )
  expect_error(new_interval(hour = NULL, minute = 30), "accepts one input")
  expect_error(new_interval(hour = 1:2, minute = 30), "accepts one input")
})

test_that("interval class", {
  int <- new_interval(hour = 1, minute = 30)
  expect_is(int, "interval")
  expect_equal(format(int), "1h 30m")
  expect_is(new_interval(), "interval")
  expect_equal(format(new_interval()), "?")
  expect_is(new_interval(.regular = FALSE), "interval")
  expect_equal(format(new_interval(.regular = FALSE)), "!")
})
