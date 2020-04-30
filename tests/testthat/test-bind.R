idx_day <- seq.Date(ymd("2017-02-01"), ymd("2017-02-05"), by = 1)
dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rnorm(10)
)

tsbl <- as_tsibble(dat_x, key = group, index = date)
tsbl2 <- mutate(tsbl, date = date + rep(5:9, each = 2), value2 = 2)

test_that("bind_rows()", {
  expect_error(bind_rows(tsbl, tsbl), "is not a valid tsibble.")
  expect_identical(bind_rows(tsbl[1, ], tsbl[-1, ]), tsbl)
  expect_is(bind_rows(tsbl, tsbl2), "tbl_ts")
})

vic <- tourism %>%
  filter(State == "Victoria")
nsw <- tourism %>%
  filter(State == "New South Wales")

test_that("bind_rows() for custom index class #78", {
  res <- bind_rows(vic, nsw)
  expect_is(res$Quarter, "yearquarter")
})

test_that("bind_rows() for mixed key properties", {
  expect_identical(
    format(interval(bind_rows(update_tsibble(vic, regular = FALSE))), nsw), "!")
  res2 <- bind_rows(
    update_tsibble(vic, regular = FALSE),
    update_tsibble(nsw, regular = FALSE)
  )
  expect_identical(format(interval(res2)), "!")
  expect_error(bind_rows(rename(vic, YrQtr = Quarter), nsw), "No common index variable")
})

test_that("bind_cols()", {
  expect_is(bind_cols(tsbl, value2 = 2), "tbl_ts")
})
