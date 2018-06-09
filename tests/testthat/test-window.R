context("Rolling window function and its variants")

x <- 1:3

test_that("slide() and slider() output", {
  expect_error(slider(x, size = 0))
  expect_error(slider(list(list(x))))
  expect_equal(slider(x), list(1, 2, 3))
  expect_equal(slider(x, size = 2), list(1:2, 2:3))
  expect_equal(slide(x, sum), 1:3)
  expect_equal(slide(x, sum, size = 2), c(NA, 3, 5))
  expect_equal(slide(x, sum, size = 2, fill = 0), c(0, 3, 5))
})

test_that("tile() and tiler() output", {
  expect_equal(tiler(x), list(1, 2, 3))
  expect_equal(tiler(x, size = 2), list(1:2, 3))
  expect_equal(tile(x, sum), 1:3)
  expect_equal(tile(x, sum, size = 2), c(3, 3))
})

test_that("stretch() and stretcher() output", {
  expect_error(stretcher(x, init = c(3, 5)))
  expect_equal(stretcher(x), list(1, 1:2, 1:3))
  expect_equal(stretcher(x, init = 2), list(1:2, 1:3))
  expect_equal(stretch(x, sum), c(1, 3, 6))
  expect_equal(stretch(x, sum, init = 2), c(3, 6))
})

sx <- pedestrian %>%
  filter(Sensor == "Southern Cross Station", Date <= as.Date("2015-01-06"))

test_that("slide_*()", {
  qtl_df <- sx %>%
    slide_dfr(~ quantile(.$Count), size = 24)
  expect_equal(dim(qtl_df), c(NROW(sx), 5))
  expect_is(qtl_df, "tbl_df")
  qtl_lst <- sx %>%
    slide_lst(~ quantile(.$Count), size = 24)
  expect_equal(NROW(qtl_df), NROW(sx))
  expect_is(qtl_lst, "list")
})

test_that("tile_*()", {
  qtl_df <- sx %>%
    tile_dfr(~ quantile(.$Count), size = 24)
  expect_equal(dim(qtl_df), c(NROW(sx) / 24, 5))
  expect_is(qtl_df, "tbl_df")
  qtl_lst <- sx %>%
    tile_lst(~ quantile(.$Count), size = 24)
  expect_equal(NROW(qtl_df), NROW(sx) / 24)
  expect_is(qtl_lst, "list")
})

test_that("stretch_*()", {
  qtl_df <- sx %>%
    stretch_dfr(~ quantile(.$Count), init = 48)
  expect_equal(dim(qtl_df), c(NROW(sx) - 47, 5))
  expect_is(qtl_df, "tbl_df")
  qtl_lst <- sx %>%
    stretch_lst(~ quantile(.$Count), init = 48)
  expect_equal(NROW(qtl_df), NROW(sx) - 47)
  expect_is(qtl_lst, "list")
})

