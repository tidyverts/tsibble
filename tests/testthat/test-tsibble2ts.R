context("tsibble to ts")

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
  expect_is(as.ts(tsbl5), "mts")
  expect_equal(NCOL(as.ts(tsbl5, value = value2)), 1)
  expect_is(as.ts(tsbl5, value = value2), "ts")
})

test_that("a tsibble with a single key", {
  x <- ts(matrix(1:10, ncol = 2))
  tsbl1 <- as_tsibble(x)
  y1 <- as.ts(x)
  expect_identical(x, y1)
})

test_that("a tsibble with more than one measured vars", {
  expect_error(as.ts(pedestrian), "Can't determine column `value`:")
  expect_error(as.ts(pedestrian, value = Date_Time), "`value` must be one of them:")
  y <- as.ts(pedestrian, value = Count)
  expect_is(y, "mts")
  expect_equal(frequency(y), 24)
  expect_equal(frequency(pedestrian), 24)
  expect_equal(ncol(y), 4)
})

test_that("time.* and guess_frequency.*", {
 dat <- seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by = 1)
 y <- time(dat)
 expect_is(y, "ts")
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
