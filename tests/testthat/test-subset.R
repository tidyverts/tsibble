context("Test a tsibble for base subset")

idx_day <- seq.Date(ymd("2017-02-01"), ymd("2017-02-05"), by = 1)
dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rnorm(10)
)

tsbl <- as_tsibble(dat_x, key = id(group), index = date)

test_that("Test if it's an atomic vector", {
  expect_is(tsbl$date, "Date")
  expect_is(tsbl[["date"]], "Date")
  expect_is(tsbl$group, "character")
  expect_is(tsbl[["group"]], "character")
  expect_is(tsbl$value, "numeric")
  expect_is(tsbl[["value"]], "numeric")
})

test_that("Test if it's a tibble", {
  expect_is(tsbl[, 1], "tbl_df")
  expect_is(tsbl[, 2], "tbl_df")
  expect_is(tsbl[, 3], "tbl_df")
  expect_is(tsbl[, 2:3], "tbl_df")
  expect_is(tsbl[, c(1, 3)], "tbl_df")
})

test_that("Test if it's a tsibble", {
  expect_is(tsbl[, c(1, 2)], "tbl_ts")
  expect_warning(tsbl[, c(1, 2), drop = TRUE])
  tsbl2 <- tsbl[1, ]
  expect_is(tsbl2, "tbl_ts")
  expect_true(is_regular(tsbl2))
  expect_is(tsbl[sample(1:10, size = 2), ], "tbl_ts")
})

dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  level = rep("z", 10),
  value = rnorm(10)
)

test_that("Subset 2 nested variable in a tsibble", {
  tsbl <- as_tsibble(dat_x, key = id(group | level), index = date)
  tsbl2 <- tsbl[, c(1, 2, 4)]
  expect_is(tsbl2, "tbl_ts")
  expect_identical(key_vars(tsbl2)[[1]], "group")
  expect_is(tsbl[, c(1, 3, 4)], "tbl_df")
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

test_that("Subset 2 crossed variable in a tsibble", {
  tsbl <- as_tsibble(dat_x, key = id(group1, group2), index = date)
  expect_is(tsbl[, c(1, 2, 4)], "tbl_df")
  expect_is(tsbl[, c(1, 3, 4)], "tbl_df")
  expect_is(tsbl[, 2:4], "tbl_df")
})
