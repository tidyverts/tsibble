context("time-wise functions")

test_that("diff() input", {
  expect_error(diff(1:10, lag = -1), "must be positive integers.")
  expect_error(diff(1:10, differences = -1), "must be positive integers.")
})

test_that("diff() output", {
  expect_equal(diff(1:10, 2), c(rep(NA, 2), base::diff(1:10, 2)))
  expect_equal(diff(1:10, 2, 2), c(rep(NA, 4), base::diff(1:10, 2, 2)))
  x <- cumsum(cumsum(1:10))
  expect_equal(diff(x, lag = 2), c(rep(NA, 2), base::diff(x, lag = 2)))
  expect_equal(diff(x, 1, 2), c(rep(NA, 2), base::diff(x, 1, 2)))
  expect_equal(diff(x, 10, 2), base::diff(x, 10, 2))
})
