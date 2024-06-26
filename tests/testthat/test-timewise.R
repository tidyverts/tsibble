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
  # expect_warning(
  #   right <- mutate(scrambled, diff = difference(value, order_by = year)), msg)
  # expect_equal(sort(right$diff, na.last = FALSE), difference(tsbl$value))
})


test_that("difference() with short time series (#310)", {
  expect_length(difference(1, differences = 3), 1L)
})
