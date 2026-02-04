test_that("a tsibble with different frequnecy", {
  x1 <- ts(1:10)
  tsbl1 <- as_tsibble(x1)
  y1 <- as.ts(tsbl1)
  expect_equal(x1, y1)
  expect_equal(frequency(y1), 1)
  expect_equal(start(y1), c(1, 1))
  x2 <- ts(1:10, start = 2000)
  tsbl2 <- as_tsibble(x2)
  y2 <- as.ts(tsbl2)
  expect_equal(x2, y2)
  expect_equal(start(y2), c(2000, 1))
  x3 <- ts(1:10, start = c(2000, 1), frequency = 4)
  tsbl3 <- as_tsibble(x3)
  y3 <- as.ts(tsbl3)
  expect_equal(x3, y3)
  expect_equal(frequency(y3), 4)
  expect_equal(start(y3), c(2000, 1))
  x4 <- ts(1:10, start = c(2000, 1), frequency = 12)
  tsbl4 <- as_tsibble(x4)
  y4 <- as.ts(tsbl4)
  expect_equal(x4, y4)
  expect_equal(frequency(y4), 12)
  expect_equal(start(y4), c(2000, 1))
  tsbl5 <- tsbl4 %>%
    mutate(value2 = rnorm(10))
  expect_equal(ncol(as.ts(tsbl5)), 2)
  expect_s3_class(as.ts(tsbl5), "mts")
  expect_equal(NCOL(as.ts(tsbl5, value = value2)), 1)
  expect_s3_class(as.ts(tsbl5, value = value2), "ts")
})

test_that("a tsibble with a single key", {
  x <- ts(matrix(1:10, ncol = 2))
  tsbl1 <- as_tsibble(x)
  y1 <- as.ts(x)
  expect_identical(x, y1)
})

test_that("a tsibble with a single key but multiple variables #261", {
  ts <- ts(5, start = 1, end = 12)
  tsbl <- tsibble(i = 1:12, q = 5, a = "x", b = "y", index = "i", key = c(a, b))
  expect_identical(as.ts(tsbl), ts)
})

harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 2012, 2014),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6), farm = 1L,
  key = fruit, index = year
)

test_that("as.ts.tbl_ts()", {
  skip_on_cran()
  expected <- ts(matrix(fill_gaps(harvest, .full = TRUE)$kilo, ncol = 2),
    start = 2010, frequency = 1)
  colnames(expected) <- c("cherry", "kiwi")
  expect_equal(as.matrix(as.ts(harvest, value = kilo)), expected)

  # Temporarily removed (#325)
  # freq <- frequency(EuStockMarkets)
  # x <- head(EuStockMarkets)
  # y <- as_tsibble(x)
  # expect_equal(
  #   as.double(as.ts(y, frequency = freq)),
  #   as.double(x)
  # )
})

test_that("a tsibble with more than one measured vars", {
  expect_error(as.ts(harvest), "Can't determine column `value`:")
  expect_error(as.ts(harvest, value = year), "`value` must be one of them:")
  y <- as.ts(harvest, value = kilo)
  expect_s3_class(y, "mts")
  expect_equal(frequency(y), 1)
  expect_equal(ncol(y), 2)
  expect_equal(nrow(y), length(unique(harvest[["year"]])))
})

test_that("as.ts.tbl_ts(fill = )", {
  expect_error(as.ts(harvest, value = kilo, fill = NULL), "not TRUE")
  y <- as.ts(harvest, value = kilo, fill = 0)
  expect_false(anyNA(y))
})

test_that("time.* and guess_frequency.*", {
  dat <- seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by = 1)
  y <- time_ts(dat)
  expect_equal(frequency(y), 7)
  expect_equal(guess_frequency(dat), 7)
  dat_min <- seq(
    as.POSIXct("2017-01-01 00:00"), as.POSIXct("2017-01-10 23:00"),
    by = "1 min"
  )
  expect_equal(guess_frequency(dat_min), 60)
  dat_sec <- seq(
    as.POSIXct("2017-01-01 00:00"), as.POSIXct("2017-01-10 23:00"),
    by = 1
  )
  expect_equal(guess_frequency(dat_sec), 60)
})

test_that("guess_frequency() for one observation #124", {
  expect_identical(guess_frequency(as.Date("2017-01-01")), 7)
  expect_identical(guess_frequency(yearmonth(as.Date("2017-01-01"))), 12)
  one_obs <- tsibble(date = as.Date("2017-01-01"), gdp = 5)
  expect_identical(frequency(as.ts(one_obs)), 7)
  expect_identical(frequency(as.ts(one_obs, frequency = 12)), 12)
})

test_that("as.ts() automatically fills implicit missings #160", {
  my_tsbl <- as_tsibble(data.frame(
    date = structure(c(18058, 18059, 18060, 18061, 18064, 18065, 18066, 18067, 18072, 18073), class = "Date"),
    val = rnorm(10)), index = date)
  expect_identical(
    as.ts(fill_gaps(my_tsbl), frequency = 365.25),
    as.ts((my_tsbl), frequency = 365.25))
})

test_that("as.ts() for multiple keys", {
  expect_error(as.ts(tourism), "the key of multiple")
})
