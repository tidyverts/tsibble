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

test_that("interval_pull.POSIXt() keeps whole days as hours (#286)", {
  origin <- as.POSIXct("2017-01-01", tz = "UTC")
  expect_identical(
    format_interval(interval_pull(origin + 86400 * 0:3)), "24h"
  )
  expect_identical(
    format_interval(interval_pull(origin + 86400 * c(0, 2, 4))), "48h"
  )
  expect_identical(
    format_interval(interval_pull(origin + 36 * 3600 * 0:3)), "36h"
  )
  expect_identical(
    default_time_units(interval_pull(origin + 86400 * 0:3)), 86400
  )

  dates <- seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1)
  tsbl <- as_tsibble(
    data.frame(time = lubridate::as_datetime(dates)),
    index = time
  )
  expect_true(is_regular(tsbl))
  expect_identical(format_interval(interval(tsbl)), "24h")
})
