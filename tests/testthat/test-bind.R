context("bind tsibble")

idx_day <- seq.Date(ymd("2017-02-01"), ymd("2017-02-05"), by = 1)
dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rnorm(10)
)

tsbl <- as_tsibble(dat_x, key = id(group), index = date)
tsbl2 <- mutate(tsbl, date = date + rep(5:9, each = 2), value2 = 2)

test_that("rbind()", {
  expect_error(rbind(tsbl, tsbl), "is not a valid tsibble.")
  expect_identical(rbind(tsbl[1, ], tsbl[-1, ]), tsbl)
  expect_is(rbind(tsbl, tsbl2), "tbl_ts")
})

test_that("cbind()", {
  expect_is(cbind(tsbl, tsbl), "tbl_ts")
})
