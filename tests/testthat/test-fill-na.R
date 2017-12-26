library(tibble)
context("Test fill_na() for a tsibble")

idx_day <- seq.Date(ymd("2017-01-01"), ymd("2017-01-20"), by = 4)
dat_x <- tibble(
  date = idx_day,
  value = rnorm(5)
)

test_that("Test a tbl_ts/data.frame", {
  expect_error(fill_na(dat_x))
})

test_that("Test a tbl_ts without implicit missing values", {
  tsbl <- as_tsibble(dat_x, index = date)
  expect_identical(fill_na(tsbl), tsbl)
})

dat_y <- dat_x[c(1:3, 5), ]
tsbl <- as_tsibble(dat_y, index = date)

test_that("Test a tbl_ts of 4 day interval with no replacement", {
  full_tsbl <- fill_na(tsbl)
  expect_identical(dim(full_tsbl), c(5L, 2L))
  expect_equal(
    as_tibble(full_tsbl[4, ]),
    tibble(date = ymd("2017-01-13"), value = NA_real_)
  )
})

test_that("Test a tbl_ts of 4 day interval with value replacement", {
  expect_error(fill_na(tsbl, value = 0L))
  full_tsbl <- fill_na(tsbl, value = 0)
  expect_equal(
    as_tibble(full_tsbl[4, ]),
    tibble(date = ymd("2017-01-13"), value = 0)
  )
})

test_that("Test a tbl_ts of 4 day interval with bad names", {
  expect_error(fill_na(tsbl, value1 = value))
})

test_that("Test a tbl_ts of 4 day interval with function replacement", {
  full_tsbl <- fill_na(tsbl, value = sum(value, na.rm = TRUE))
  expect_equal(
    as_tibble(full_tsbl[4, ]),
    tibble(date = ymd("2017-01-13"), value = sum(tsbl$value))
  )
})

dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rep(1:2, each = 5)
)
dat_y <- dat_x[c(2:8, 10), ]
tsbl <- as_tsibble(dat_y, key = id(group), index = date)

test_that("Test grouped_ts", {
  full_tsbl <- tsbl %>%
    group_by(group) %>%
    fill_na(value = sum(value, na.rm = TRUE))
  expect_identical(dim(full_tsbl), c(10L, 3L))
  expect_equal(
    as_tibble(full_tsbl[c(1, 9), ]),
    tibble(
      date = c(ymd("2017-01-01"), ymd("2017-01-13")),
      group = c("a", "b"),
      value = c(4L, 8L)
    )
  )
})
