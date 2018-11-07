context("append_row() for a tsibble")

library(lubridate)
idx_day <- seq.Date(ymd("2017-01-01"), ymd("2017-01-20"), by = 4)
dat_x <- tibble(
  date = idx_day,
  value = rnorm(5)
)

test_that("a tbl_df/data.frame", {
  expect_error(append_row(dat_x), "`tbl_df`")
})

test_that("unknown interval", {
  tsbl <- as_tsibble(dat_x[1, ], index = date)
  expect_error(append_row(tsbl), "data of unknown interval.")
})

test_that("an irregular tbl_ts", {
  tsbl <- as_tsibble(dat_x, index = date, regular = FALSE)
  expect_error(append_row(tsbl), "irregular")
})

test_that("4 day interval", {
  tsbl <- as_tsibble(dat_x, index = date)
  ref_tbl <- tibble(date = ymd("2017-01-21"), value = NA_real_)
  expect_equal(append_row(tsbl)[6, ], ref_tbl)
  expect_error(append_row(tsbl, 1:3), "a positive")
})

tourism <- tourism %>% 
  group_by_key() %>% 
  slice(1:3) %>% 
  ungroup()

test_that("custom index class", {
  new_t <- append_row(tourism)
  expect_equal(NROW(new_t), NROW(tourism) + n_keys(tourism))
  expect_equal(new_t[["Trips"]][4], NA_real_)
})

test_that("ordered?", {
  expect_warning(unord_t <- tourism %>% arrange(Trips))
  expect_warning(new_t <- append_row(unord_t))
  expect_equal(
    new_t[["Trips"]][(NROW(tourism) + 1):NROW(new_t)], 
    rep(NA_real_, n_keys(tourism)))
})

test_that("`.keep_all = TRUE", {
  new_t <- new_data(tourism, .keep_all = TRUE)
  expect_equal(dim(new_t), c(308, 5))
})
