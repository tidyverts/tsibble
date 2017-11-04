library(rlang)
library(tibble)
library(lubridate)
library(tsibble)

context("Test as_tsibble() w/o key for data of long form")

idx_second <- seq(
  ymd_hms("2017-01-01 00:00:00"), 
  ymd_hms("2017-01-01 00:00:04"),
  by = 1
)
dat_x <- tibble(
  date_time = idx_second, 
  value = rnorm(5)
)

test_that("POSIXt with 1 second interval", {
  expect_identical(index_sum(dat_x$date_time), "dttm")
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_is(index(tsbl), "quosure")
  expect_identical(quo_text(index(tsbl)), "date_time")
  expect_identical(format(key(tsbl)), "NULL")
  expect_identical(format(groups(tsbl)), "NULL")
  expect_identical(format(interval(tsbl)), "1SECOND")
  expect_true(is_regular(tsbl))
  tsbl1 <- rename(tsbl, `Date Time` = date_time)
  expect_identical(quo_text(index(tsbl1)), "Date Time")
  dat_y <- dat_x[c(1, 1, 3:5), ]
  expect_error(as_tsibble(dat_y, index = date_time))
})

test_that("POSIXt with an unrecognisable interval", {
  dat_y <- dat_x[1, ]
  tsbl <- as_tsibble(dat_y, index = date_time)
  expect_identical(format(interval(tsbl)), "?")
})

test_that("POSIXt with irregular interval", {
  dat_y <- dat_x[1, ]
  tsbl <- as_tsibble(dat_y, index = date_time, regular = FALSE)
  expect_identical(format(interval(tsbl)), "!")
  expect_false(is_regular(tsbl))
})

idx_minute <- seq.POSIXt(
  ymd_hm("2017-01-01 00:00"),
  ymd_hm("2017-01-01 00:08"),
  by = "2 min"
)

dat_x <- tibble(
  date_time = idx_minute,
  value = rnorm(5)
)

test_that("POSIXt with 2 minutes interval", {
  tsbl <- as_tsibble(dat_x)
  expect_identical(format(interval(tsbl)), "2MINUTE")
})

idx_hour <- seq.POSIXt(
  ymd_h("2017-01-01 0"), ymd_h("2017-01-01 12"), by = "3 hour"
)

dat_x <- tibble(
  date_time = idx_hour,
  value = rnorm(5)
)

test_that("POSIXt with 3 hours interval", {
  tsbl <- as_tsibble(dat_x)
  expect_identical(format(interval(tsbl)), "3HOUR")
})

idx_day <- seq.Date(ymd("2017-01-01"), ymd("2017-01-20"), by = 4)

dat_x <- tibble(
  date = idx_day,
  value = rnorm(5)
)

test_that("Date with 4 days interval", {
  expect_identical(index_sum(dat_x$date), "date")
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "4DAY")
})

idx_month <- seq(yearmth(ymd("2017-01-01")), yearmth(ymd("2017-05-01")), by = 1)
dat_x <- tibble(
  yrmth = idx_month,
  value = rnorm(5)
)

test_that("Year month with 1 month interval", {
  expect_identical(index_sum(dat_x$yrmth), "mth")
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "1MONTH")
})

idx_qtr <- seq(yearqtr(ymd("2016-01-01")), yearqtr(ymd("2017-01-01")), by = 1)
dat_x <- tibble(
 yrqtr = idx_qtr,
 value = rnorm(5)
)

test_that("Year quarter with 1 quarter interval", {
  expect_identical(index_sum(dat_x$yrqtr), "qtr")
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "1QUARTER")
})

idx_year <- year(seq.int(1970, 2010, by = 10))
dat_x <- tibble(
 year = idx_year,
 value = rnorm(5)
)

test_that("Year with 10 years interval", {
  expect_identical(index_sum(dat_x$year), "int")
  expect_error(as_tsibble(dat_x))
  tsbl <- as_tsibble(dat_x, index = year)
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "10YEAR")
})

library(hms)
idx_time <- hms(hour = rep(0, 5), minutes = 1:5, second = rep(0, 5))
dat_x <- tibble(time = idx_time, value = rnorm(5))

test_that("Difftime with 1 minute interval", {
  expect_identical(index_sum(dat_x$time), "time")
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "1MINUTE")
})
