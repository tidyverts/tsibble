context("a tsibble for base subset")

idx_day <- seq.Date(ymd("2017-02-01"), ymd("2017-02-05"), by = 1)
dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rnorm(10)
)

tsbl <- as_tsibble(dat_x, key = id(group), index = date)

test_that("if it's an atomic vector", {
  expect_is(tsbl$date, "Date")
  expect_is(tsbl[["date"]], "Date")
  expect_is(tsbl$group, "character")
  expect_is(tsbl[["group"]], "character")
  expect_is(tsbl$value, "numeric")
  expect_is(tsbl[["value"]], "numeric")
})

test_that("if it's a tibble", {
  expect_is(tsbl[, 1], "tbl_df")
  expect_is(tsbl[, 2], "tbl_df")
  expect_is(tsbl[, 3], "tbl_df")
  expect_is(tsbl[, 2:3], "tbl_df")
  expect_is(tsbl[, c(1, 3)], "tbl_df")
})

tsbl1 <- tsibble(date = as.Date("2010-01-01") + 0:10)
test_that("if it's a vector", {
  expect_equal(tsbl1[, 1, drop = TRUE], tsbl1$date)
  tsbl2 <- tsbl1[1, ]
  expect_equal(tsbl2[, 1, drop = TRUE], as.Date("2010-01-01"))
})

test_that("if it's a tsibble", {
  expect_error(tsbl[1:11, ], "exceed the number of rows")
  expect_equal(tsbl[], tsbl)
  expect_is(tsbl[, c(1, 2)], "tbl_ts")
  expect_warning(tsbl[c("group", "date"), drop = TRUE], "`drop` is ignored.")
  expect_identical(tsbl[FALSE, ], tsbl[0, ])
  expect_identical(tsbl[TRUE, ], tsbl)
  tsbl2 <- tsbl[1, ]
  expect_is(tsbl2, "tbl_ts")
  expect_true(is_regular(tsbl2))
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
  expect_is(tsbl2[c("date", "group", "value")], "tbl_ts")
  expect_identical(key_vars(tsbl2)[[1]], "group")
  expect_is(tsbl[, c(1, 3, 4)], "tbl_df")
  expect_is(tsbl[c("date", "level", "value")], "tbl_df")
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
