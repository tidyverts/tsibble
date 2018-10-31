context("as_tsibble() w/o key for data of long form")

idx_second <- seq(
  ymd_hms("2017-01-01 00:00:00"),
  ymd_hms("2017-01-01 00:00:04"),
  by = 1
)
dat_x <- tibble(
  date_time = idx_second,
  value = rnorm(5)
)

test_that("A tsibble cannot be NULL or without index", {
  expect_error(tsibble(), "Can't determine the index")
  expect_error(as_tsibble(), "NULL")
})

test_that("A tsibble must not contain missing values in index", {
  expect_error(
    tsibble(idx = ymd_h("2017-10-01 0", tz = "Australia/Melbourne") + hours(1:3)),
    "must not contain `NA`.")
})

test_that("A tibble is not tsibble", {
  expect_false(is_tsibble(dat_x))
  expect_error(key(dat_x), "Can't find the attribute key")
  expect_error(index(dat_x), "tsibble")
  expect_error(interval(dat_x), "tsibble")
  expect_error(is_regular(dat_x), "tsibble")
})

test_that("Coerce to tbl_df and data.frame", {
  expect_error(tsibble(dat_x), "not be a data frame,")
  tsbl <- as_tsibble(dat_x, index = date_time)
  expect_identical(as_tibble(tsbl), dat_x)
  expect_identical(as.data.frame(tsbl), as.data.frame(dat_x))
})

start <- as.POSIXct("2010-01-15 13:55:23.975", tz = "UTC")
x <-  start + lubridate::milliseconds(x = seq(0, 99, by = 5))
df <- data.frame(time = x, value = rnorm(length(x)))
tsbl <- as_tsibble(df, index = time)

test_that("POSIXct with 10 milliseconds interval", {
  expect_output(print(tsbl), "A tsibble: 20 x 2 \\[5ms\\]")
})

x <- ISOdatetime(2011,8,2,0,0,0) + c(34201881660:34201881669)*1e-6
df <- data.frame(time = x, value = rnorm(10))
tsbl <- as_tsibble(df, index = time)

test_that("POSIXct with 1 microseconds interval", {
  expect_output(print(tsbl), cat("A tsibble: 10 x 2 [10\U00B5s]"))
})

library(nanotime)
x <- nanotime("1970-01-01T00:00:00.000000001+00:00") + 0:9
df <- data.frame(time = x, value = rnorm(10))
tsbl <- as_tsibble(df)

test_that("nanotime with 1 nanoseconds interval", {
  expect_output(print(tsbl), "A tsibble: 10 x 2 \\[1ns\\]")
})

test_that("POSIXt with 1 second interval", {
  expect_identical(index_valid(dat_x$date_time), TRUE)
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_output(print(tsbl), "A tsibble: 5 x 2 \\[1s\\]")
  expect_error(as_tsibble(dat_x, key = id(date_time)))
  expect_is(tsbl, "tbl_ts")
  expect_is(index(tsbl), "name")
  expect_identical(quo_text(index(tsbl)), "date_time")
  expect_identical(time_unit(pull_interval(tsbl$date_time)), 1)
  expect_identical(key(tsbl), list())
  expect_identical(format(groups(tsbl)), "NULL")
  expect_identical(format(interval(tsbl)), "1s")
  expect_output(print(interval(tsbl)), "1s")
  expect_true(is_regular(tsbl))
  expect_equal(key_size(tsbl), 5)
  expect_equal(n_keys(tsbl), 1)
  expect_equal(group_size(tsbl), 5)
})

test_that("Space in index variable", {
  tbl <- rename(dat_x, `Date Time` = date_time)
  tsbl <- as_tsibble(tbl)
  expect_identical(as_string(index(tsbl)), "Date Time")

})

test_that("Duplicated time index", {
  dat_y <- dat_x[c(1, 1, 3:5), ]
  expect_error(as_tsibble(dat_y, index = date_time), "A valid tsibble")

  y <- c(FALSE, TRUE, rep(FALSE, 3))
  expect_identical(find_duplicates(dat_y, index = date_time), y)
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
  expect_identical(format(interval(tsbl)), "2m")
  expect_identical(time_unit(pull_interval(tsbl$date_time)), 120)
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
  expect_identical(format(interval(tsbl)), "3h")
  expect_identical(time_unit(pull_interval(tsbl$date_time)), 3 * 60 * 60)
})

idx_day <- seq.Date(ymd("2017-01-01"), ymd("2017-01-20"), by = 4)

dat_x <- tibble(
  date = idx_day,
  value = rnorm(5)
)

test_that("Date with 4 days interval", {
  expect_identical(index_valid(dat_x$date), TRUE)
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "4D")
  expect_identical(time_unit(pull_interval(tsbl$date)), 4)
})

idx_week <- seq(yearweek(ymd("2017-02-01")), length.out = 5, by = 1)
dat_x <- tibble(yrwk = idx_week, value = rnorm(5))

test_that("Year week with 1 week interval", {
  expect_identical(index_valid(dat_x$yrwk), TRUE)
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_output(print(tsbl), "A tsibble: 5 x 2 \\[1W\\]")
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "1W")
  expect_identical(time_unit(pull_interval(tsbl$yrwk)), 1)
})

idx_month <- seq(
  yearmonth(ymd("2017-01-01")), yearmonth(ymd("2017-05-01")), by = 1
)
dat_x <- tibble(
  yrmth = idx_month,
  value = rnorm(5)
)

test_that("Year month with 1 month interval", {
  expect_identical(index_valid(dat_x$yrmth), TRUE)
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_output(print(tsbl), "A tsibble: 5 x 2 \\[1M\\]")
  expect_is(tsbl, "tbl_ts")
  expect_is(as_tsibble(tsbl, validate = TRUE), "tbl_ts")
  expect_identical(format(interval(tsbl)), "1M")
  expect_identical(time_unit(pull_interval(tsbl$yrmth)), 1)
})

idx_qtr <- seq(
  yearquarter(ymd("2016-01-01")), yearquarter(ymd("2017-01-01")), by = 1
)
dat_x <- tibble(
 yrqtr = idx_qtr,
 value = rnorm(5)
)

test_that("Year quarter with 1 quarter interval", {
  expect_identical(index_valid(dat_x$yrqtr), TRUE)
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "1Q")
  expect_identical(time_unit(pull_interval(tsbl$yrqtr)), 1)
})

idx_year <- seq.int(1970, 2010, by = 10)
dat_x <- tibble(
 year = idx_year,
 value = rnorm(5)
)

test_that("Year with 10 years interval", {
  expect_identical(index_valid(dat_x$year), NA)
  expect_error(as_tsibble(dat_x))
  tsbl <- as_tsibble(dat_x, index = year)
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "10Y")
  expect_identical(time_unit(pull_interval(tsbl$year)), 10)
})

library(hms)
idx_time <- hms(hour = rep(0, 5), minutes = 1:5, second = rep(0, 5))
dat_x <- tibble(time = idx_time, value = rnorm(5))

test_that("Difftime with 1 minute interval", {
  expect_identical(index_valid(dat_x$time), TRUE)
  expect_message(tsbl <- as_tsibble(dat_x))
  expect_is(tsbl, "tbl_ts")
  expect_identical(format(interval(tsbl)), "1m")
})

context("as_tsibble() with a single key for data of long form")

idx_day <- seq.Date(ymd("2017-02-01"), ymd("2017-02-05"), by = 1)
dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rnorm(10)
)

test_that("A single key", {
  expect_error(as_tsibble(dat_x, index = date), "A valid tsibble")
  expect_error(as_tsibble(dat_x, key = "group", index = date), "Key must be created")
  tsbl <- as_tsibble(dat_x, key = id(group), index = date)
  expect_output(print(tsbl), "A tsibble: 10 x 3 \\[1D\\]")
  expect_identical(format(groups(tsbl)), "NULL")
  expect_equal(key_size(tsbl), c(5, 5))
  expect_equal(n_keys(tsbl), 2)
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

dat_x <- tribble(
  ~ date, ~ group1, ~ group2, ~ value,
  ymd("2017-10-01"), "a", "x", 1,
  ymd("2017-10-02"), "a", "x", 1,
  ymd("2017-10-01"), "a", "y", 3,
  ymd("2017-10-02"), "a", "y", 3,
  ymd("2017-10-01"), "b", "x", 2,
  ymd("2017-10-02"), "b", "y", 2
)

test_that("multiple variables", {
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

tbl <- tibble::tibble(
    mth = rep(yearmonth(seq(2017, 2017 + 9 / 12, by = 1 / 12)), 3),
    group = rep(c("x", "y", "z"), each = 10),
    value = rnorm(30)
  ) %>%
  group_by(group)

test_that("as_tsibble.tbl_ts & as_tsibble.grouped_df", {
  ped <- as_tsibble(pedestrian)
  expect_identical(ped, pedestrian)
  grped_ped <- pedestrian %>% group_by(Date)
  expect_equal(as_tsibble(grped_ped), grped_ped)
  expect_is(as_tsibble(tbl, key = id(group), index = mth), "tbl_ts")
  expect_is(as_tsibble(tbl, key = id(group), index = mth, groups = id(group)), "grouped_ts")
})

test_that("build_tsibble()", {
  expect_error(build_tsibble(
    pedestrian, key = id(Sensor), index = Date_Time,
    interval = list(hour = 1)
  ), "Argument `interval` must be class interval,")
  expect_error(
    build_tsibble(pedestrian, key = Sensor, index = Date_Time),
    "Key must be created"
  )
  expect_error(
    build_tsibble(pedestrian, key = dplyr::vars(Sensor), index = Date_Time),
    "Please use"
  )

  tsbl <- build_tsibble(
    pedestrian, key = id(Sensor), index = Date_Time,
    index2 = Date
  )
  idx2 <- index2(tsbl)
  expect_is(idx2, "name")

  idx_drop <- dplyr::bind_rows(tsbl, tsbl)
  expect_error(print(idx_drop), "dropped somehow")

  expect_error(
    build_tsibble(pedestrian, key = id(Sensor), index = NULL), "NULL."
  )

  expect_error(pedestrian %>%
    mutate(Date_Time = as.character(Date_Time)),
    "Unsupported index"
  )
})
