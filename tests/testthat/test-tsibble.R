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

test_that("A tibble is not tsibble", {
  expect_false(is_tsibble(dat_x))
  expect_error(key(dat_x))
  expect_error(index(dat_x))
  expect_error(interval(dat_x))
  expect_error(is_regular(dat_x))
})

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
})

test_that("Space in index variable", {
  tbl <- rename(dat_x, `Date Time` = date_time)
  tsbl <- as_tsibble(tbl)
  expect_identical(quo_text(index(tsbl)), "Date Time")

})

test_that("Duplicated time index", {
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

idx_month <- seq(
  yearmonth(ymd("2017-01-01")), yearmonth(ymd("2017-05-01")), by = 1
)
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

idx_qtr <- seq(
  yearquarter(ymd("2016-01-01")), yearquarter(ymd("2017-01-01")), by = 1
)
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

idx_year <- seq.int(1970, 2010, by = 10)
dat_x <- tibble(
 year = idx_year,
 value = rnorm(5)
)

test_that("Year with 10 years interval", {
  expect_identical(index_sum(dat_x$year), "dbl")
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

context("Test as_tsibble() with a single key for data of long form")

idx_day <- seq.Date(ymd("2017-02-01"), ymd("2017-02-05"), by = 1)
dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rnorm(10)
)

test_that("A single key", {
  expect_error(as_tsibble(dat_x, index = date))
  tsbl <- as_tsibble(dat_x, key = id(group), index = date)
  expect_is(key(tsbl), "key")
  expect_identical(format(key(tsbl))[[1]], "group")
  expect_identical(format(groups(tsbl)), "NULL")
})

test_that("Duplicated identifier: index", {
  dat_y <- dat_x[c(1, 2, 1, 4:10), ]
  expect_error(as_tsibble(dat_y, key = id(group), index = date))
})

test_that("Duplicated identifier: key", {
  dat_x$group <- rep(letters[1:2], c(6, 4))
  expect_error(as_tsibble(dat_x, key = id(group), index = date))
})

test_that("validate = FALSE", {
  dat_x$group <- rep(letters[1:2], c(6, 4))
  tsbl <- as_tsibble(dat_x, key = id(group), index = date, validate = FALSE)
  expect_is(tsbl, "tbl_ts")
})

dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  level = rep("z", 10),
  value = rnorm(10)
)

test_that("2 nested variable", {
  expect_error(as_tsibble(dat_x, key = id(level | group), index = date))
  tsbl <- as_tsibble(dat_x, key = id(group | level), index = date)
  expect_identical(length(key(tsbl)), 1L)
  expect_identical(length(key(tsbl)[[1]]), 2L)
  expect_identical(key_vars(tsbl)[[1]], "group | level")
})

dat_x <- tribble(
  ~ date, ~ group1, ~ group2, ~ value,
  ymd("2017-10-01"), "a", "x", 1,
  ymd("2017-10-02"), "a", "x", 1,
  ymd("2017-10-01"), "a", "y", 3,
  ymd("2017-10-02"), "a", "y", 3,
  ymd("2017-10-01"), "b", "x", 2,
  ymd("2017-10-02"), "b", "y", 2
)

test_that("2 crossed variable", {
  expect_error(as_tsibble(dat_x, key = id(group1), index = date))
  expect_error(as_tsibble(dat_x, key = id(group2), index = date))
  tsbl <- as_tsibble(dat_x, key = id(group1, group2), index = date)
  expect_identical(length(key(tsbl)), 2L)
})

test_that("Use '-' and ':' in key vars", {
  tsbl1 <- as_tsibble(dat_x, key = id(-date, -value), index = date)
  expect_identical(length(key(tsbl1)), 2L)
  tsbl2 <- as_tsibble(dat_x, key = id(group1:group2), index = date)
  expect_identical(length(key(tsbl2)), 2L)
})

dat_x <- tribble(
  ~ date, ~ bottom, ~ group1, ~ group2, ~ value,
  ymd("2017-10-01"), 1, "a", "x", 1,
  ymd("2017-10-02"), 1, "a", "x", 1,
  ymd("2017-10-01"), 2, "a", "x", 1,
  ymd("2017-10-02"), 2, "a", "x", 1,
  ymd("2017-10-01"), 1, "a", "y", 1,
  ymd("2017-10-02"), 1, "a", "y", 1,
  ymd("2017-10-01"), 2, "a", "y", 1,
  ymd("2017-10-02"), 2, "a", "y", 1,
  ymd("2017-10-01"), 3, "b", "y", 3,
  ymd("2017-10-02"), 3, "b", "y", 3,
  ymd("2017-10-01"), 3, "b", "x", 3,
  ymd("2017-10-02"), 3, "b", "x", 3,
  ymd("2017-10-01"), 4, "b", "y", 3,
  ymd("2017-10-02"), 4, "b", "y", 3,
  ymd("2017-10-01"), 4, "b", "x", 3,
  ymd("2017-10-02"), 4, "b", "x", 3
)

test_that("2 nested variables crossed with 1 variable", {
  expect_error(as_tsibble(dat_x, key = id(bottom | group1), index = date))
  expect_error(as_tsibble(dat_x, key = id(group1, group2), index = date))
  tsbl <- as_tsibble(dat_x, key = id(bottom | group1, group2), index = date)
  expect_identical(length(key(tsbl)), 2L)
  expect_identical(length(key(tsbl))[[1]], 2L)
  expect_identical(key_vars(tsbl)[[1]], "bottom | group1")
  expect_identical(key_vars(tsbl)[[2]], "group2")
})

colnames(dat_x) <- c("1", "Bottom 1", "Group 1", "Group 2", "Value X")

test_that("Spectial characters in column names", {
  tsbl <- as_tsibble(
    dat_x, key = id(`Bottom 1` | `Group 1`, `Group 2`), index = `1`
  )
  expect_identical(key_vars(tsbl)[[1]], "`Bottom 1` | `Group 1`")
  expect_identical(key_vars(tsbl)[[2]], "Group 2")
})
