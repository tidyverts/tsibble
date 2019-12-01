context("time-wise functions")

test_that("difference() input", {
  expect_error(difference(1:10, lag = -1), "must be positive integers.")
  expect_error(difference(1:10, differences = -1), "must be positive integers.")
})

test_that("difference() output", {
  expect_equal(difference(1:10, 2), c(rep(NA, 2), diff(1:10, 2)))
  expect_equal(difference(1:10, 2, 2), c(rep(NA, 4), diff(1:10, 2, 2)))
  x <- cumsum(cumsum(1:10))
  expect_equal(difference(x, lag = 2), c(rep(NA, 2), diff(x, lag = 2)))
  expect_equal(difference(x, 1, 2), c(rep(NA, 2), diff(x, 1, 2)))
  # expect_equal(difference(x, 10, 2), diff(x, 10, 2))
})

tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)

test_that("difference() with `order_by`", {
  msg <- "Current temporal order."
  expect_warning(scrambled <- tsbl %>% slice(sample(nrow(.))), msg)
  expect_warning(
    right <- mutate(scrambled, diff = difference(value, order_by = year)),
    msg
  )
  expect_equal(sort(right$diff, na.last = FALSE), difference(tsbl$value))
})

yrmth <- yearmonth(c("1995-07", "1995-08", "1995-11", "1995-12", "1996-01", "1996-03"))
vals <- c(1153, 1181, 1236, 1297, 1265, 1282)
tsbl <- tsibble(mdate = yrmth, income = vals, index = mdate)

test_that("keyed_[lag/lead/difference] error", {
  expect_error(
    tsbl %>% mutate(l_income = keyed_lag(income, n = -2)), 
    "non-negative")
  expect_error(
    tsbl %>% mutate(l_income = keyed_lag("income")), 
    "unquoted")
  expect_error(
    tsbl %>% mutate(d_income = keyed_difference(income, lag = -2)), 
    "positive")
})

test_that("keyed_[lag/lead/difference] result", {
  res <- tsbl %>% 
    mutate(
      lag_income = keyed_lag(income),
      lead_income = keyed_lead(income),
      diff_income = keyed_difference(income)
    )
  expect_equal(res$lag_income, c(NA, 1153, NA, 1236, 1297, NA ))
  expect_equal(res$lead_income, c(1181, NA, 1297, 1265, NA, NA))
  expect_equal(res$diff_income, c(NA, -28, NA, -61, 32, NA))
})

test_that("keyed_[lag/lead/difference] for non-ordered data", {
  res <- tsbl[6:1, ] %>% 
    mutate(
      lag_income = keyed_lag(income),
      lead_income = keyed_lead(income),
      diff_income = keyed_difference(income)
    )
  expect_equal(res$lag_income, rev(c(NA, 1153, NA, 1236, 1297, NA )))
  expect_equal(res$lead_income, rev(c(1181, NA, 1297, 1265, NA, NA)))
  expect_equal(res$diff_income, rev(c(NA, -28, NA, -61, 32, NA)))
})
