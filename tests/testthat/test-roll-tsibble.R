context("rolling tsibble")

harvest <- tsibble(
  year = c(rep(2010:2012, 2), 2013),
  fruit = c(rep(c("kiwi", "cherry"), each = 3), "kiwi"),
  kilo = sample(1:10, size = 7),
  key = fruit, index = year
)

test_that("slide_tsibble()", {
  res <- slide_tsibble(harvest, .size = 2)
  expect_is(res, "list")
  expect_is(res[[1]], "tbl_ts")
  expect_length(res, 3)
  expect_equal(NROW(res[[1]]), 2 * 2)
  expect_equal(NROW(res[[3]]), 2)
})

test_that("tile_tsibble()", {
  res <- tile_tsibble(harvest, .size = 2)
  expect_length(res, 2)
  expect_equal(NROW(res[[1]]), 2 * 2)
  expect_equal(NROW(res[[2]]), 2 + 1)
})

test_that("stretch_tsibble()", {
  res <- stretch_tsibble(harvest)
  expect_length(res, 4)
  expect_equal(NROW(res[[1]]), 2)
  expect_equal(NROW(res[[4]]), 4)
})
