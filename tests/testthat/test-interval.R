test_that("multiple arguments matching", {
  expect_error(
    new_interval(y = 1, m = 2, mi = 3, second = 4),
    "Invalid argument:"
  )
  expect_error(new_interval(hour = NULL, minute = 30), "accepts one input")
  expect_error(new_interval(hour = 1:2, minute = 30), "accepts one input")
})

int <- new_interval(hour = 1, minute = 30)

test_that("interval class", {
  expect_s3_class(int, "interval")
  expect_equal(format(int), "1h 30m")
  expect_s3_class(new_interval(), "interval")
  expect_equal(format(new_interval()), "?")
  expect_s3_class(new_interval(.regular = FALSE), "interval")
  expect_equal(format(new_interval(.regular = FALSE)), "!")
})

test_that("as.period() & as.duration()", {
  expect_identical(
    lubridate::as.period(int), lubridate::period(hour = 1, minute = 30)
  )
  expect_identical(
    lubridate::as.duration(int),
    lubridate::as.duration(lubridate::period(hour = 1, minute = 30))
  )
})
