context("filter_index()")

x <- c("2016", "2016-10", "2016-12-09")

test_that("class: Date", {
  expect_equal(start(pedestrian$Date), as.Date("2015-01-01"))
  expect_equal(
    start(pedestrian$Date, x),
    as.Date(c("2016-01-01", "2016-10-01", "2016-12-09"))
  )
  expect_equal(end(pedestrian$Date), as.Date("2017-01-01"))
  expect_equal(
    end(pedestrian$Date, x),
    as.Date(c("2017-01-01", "2016-11-01", "2016-12-10"))
  )
})

y <- c("2016", "2016-10", "2016-12-09", "2016-12-09 10")
tz <- "Australia/Melbourne"

test_that("class: POSIXct", {
  expect_equal(start(pedestrian$Date_Time), as.POSIXct("2015-01-01", tz = tz))
  expect_equal(
    start(pedestrian$Date_Time, y),
    as.POSIXct(
      c("2016-01-01 00:00", "2016-10-01 00:00", "2016-12-09 00:00", "2016-12-09 10:00"),
      tz = tz
    )
  )
  expect_equal(end(pedestrian$Date_Time), as.POSIXct("2017-01-01", tz = tz))
  expect_equal(
    end(pedestrian$Date_Time, y),
    as.POSIXct(
      c("2017-01-01 00:00", "2016-11-01 00:00", "2016-12-09 01:00", "2016-12-09 11:00"),
      tz = tz
    )
  )
})
