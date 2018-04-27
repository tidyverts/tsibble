library(tibble)
context("Test fill_na() & count_gaps() for a tsibble")

idx_day <- seq.Date(ymd("2017-01-01"), ymd("2017-01-20"), by = 4)
dat_x <- tibble(
  date = idx_day,
  value = rnorm(5)
)

test_that("Test a tbl_df/data.frame", {
  expect_error(fill_na(dat_x), "data.frame")
})

test_that("Test an irregular tbl_ts", {
  tsbl <- as_tsibble(dat_x, index = date, regular = FALSE)
  expect_error(fill_na(tsbl), "irregular")
  expect_error(count_gaps(tsbl), "irregular")
})

test_that("Test a tbl_ts without implicit missing values", {
  tsbl <- as_tsibble(dat_x, index = date)
  expect_identical(fill_na(tsbl), tsbl)
  ref_tbl <- tibble(from = NA, to = NA, n = 0L)
  expect_identical(count_gaps(tsbl), ref_tbl)
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
  expect_error(fill_na(tsbl, value = 0L), "must be type integer")
  full_tsbl <- fill_na(tsbl, value = 0)
  expect_equal(
    as_tibble(full_tsbl[4, ]),
    tibble(date = ymd("2017-01-13"), value = 0)
  )
})

test_that("Test a tbl_ts of 4 day interval with bad names", {
  expect_error(fill_na(tsbl, value1 = value), "Can't find column")
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
    fill_na(value = sum(value, na.rm = TRUE), .full = TRUE)
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

test_that("Test fill.tbl_ts(.full = TRUE)", {
  full_tsbl <- tsbl %>%
    fill_na(.full = TRUE) %>%
    group_by(group) %>%
    fill(value, .direction = "up")
  expect_equal(
    as_tibble(full_tsbl[c(1, 9), ]),
    tibble(
      date = c(ymd("2017-01-01"), ymd("2017-01-13")),
      group = c("a", "b"),
      value = c(1L, 2L)
    )
  )
})

test_that("Test fill.tbl_ts(.full = FALSE)", {
  full_tsbl <- tsbl %>%
    fill_na() %>%
    group_by(group) %>%
    fill(value, .direction = "up")
  expect_equal(
    as_tibble(full_tsbl[8, ]),
    tibble(
      date = ymd("2017-01-13"),
      group = "b",
      value = 2L
    )
  )
})

test_that("Test count_gaps(.full = TRUE)", {
  full_tbl <- tsbl %>% count_gaps(.full = TRUE)
  expect_equal(
    full_tbl,
    tibble(
      group = c("a", "b"), 
      from = c(ymd("2017-01-01"), ymd("2017-01-13")), 
      to = c(ymd("2017-01-01"), ymd("2017-01-13")), 
      n = c(1L, 1L)
    )
  )
})

test_that("Test count_gaps(.full = FALSE)", {
  full_tbl <- tsbl %>% count_gaps()
  a <- tibble(group = "a", from = NA, to = NA, n = 0L)
  b <- tibble(
    group = "b", 
    from = ymd("2017-01-13"), 
    to = ymd("2017-01-13"), 
    n = 1L
  )
  expect_equal(
    full_tbl,
    dplyr::bind_rows(a, b)
  )
})
