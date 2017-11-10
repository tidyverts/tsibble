context("Test rolling window function and its variants")

x <- 1:3

test_that("Test slide.numeric() and slider() output", {
  expect_equal(slider(x), list(1, 2, 3))
  expect_equal(slider(x, size = 2), list(1:2, 2:3))
  expect_equal(slide(x, sum), 1:3)
  expect_equal(slide(x, sum, size = 2), c(NA, 3, 5))
  expect_equal(slide(x, sum, size = 2, fill = 0), c(0, 3, 5))
})

test_that("Test tile.numeric() and tiler() output", {
  expect_equal(tiler(x), list(1, 2, 3))
  expect_equal(tiler(x, size = 2), list(1:2, 3))
  expect_equal(tile(x, sum), 1:3)
  expect_equal(tile(x, sum, size = 2), c(3, 3))
})

test_that("Test stretch.numeric() and stretcher() output", {
  expect_equal(stretcher(x), list(1, 1:2, 1:3))
  expect_equal(stretcher(x, init = 2), list(1:2, 1:3))
  expect_equal(stretch(x, sum), c(1, 3, 6))
  expect_equal(stretch(x, sum, init = 2), c(3, 6))
})
