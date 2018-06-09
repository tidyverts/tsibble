context("Rolling window function and its variants")

x <- 1:3

test_that("slide() and slider() output", {
  expect_error(slider(x, .size = 0))
  expect_error(slider(list(list(x))))
  expect_equal(slider(x), list(1, 2, 3))
  expect_equal(slider(x, .size = 2), list(1:2, 2:3))
  expect_equal(slide(x, sum), 1:3)
  expect_equal(slide(x, sum, .size = 2), c(NA, 3, 5))
  expect_equal(slide(x, sum, .size = 2, .fill = 0), c(0, 3, 5))
})

test_that("tile() and tiler() output", {
  expect_equal(tiler(x), list(1, 2, 3))
  expect_equal(tiler(x, .size = 2), list(1:2, 3))
  expect_equal(tile(x, sum), 1:3)
  expect_equal(tile(x, sum, .size = 2), c(3, 3))
})

test_that("stretch() and stretcher() output", {
  expect_error(stretcher(x, .init = c(3, 5)))
  expect_equal(stretcher(x), list(1, 1:2, 1:3))
  expect_equal(stretcher(x, .init = 2), list(1:2, 1:3))
  expect_equal(stretch(x, sum), c(1, 3, 6))
  expect_equal(stretch(x, sum, .init = 2), c(3, 6))
})

y <- 3:1

test_that("slide2()", {
  expect_equal(slide2(x, y, sum), rep(4, 3))
  expect_equal(slide2(x, y, cor, .size = 2), c(NA, -1, -1))
  expect_equal(slide2(x, y, cor, .size = 2, .fill = 0), c(0, -1, -1))
})

test_that("tile2()", {
  expect_equal(tile2(x, y, sum), rep(4, 3))
  expect_equal(tile2(x, y, sum, .size = 2), c(8, 4))
  expect_equal(tile2(x, y, cor, .size = 2), c(-1, NA))
})

test_that("stretch2()", {
  expect_equal(stretch2(x, y, sum), c(4, 8, 12))
  expect_equal(stretch2(x, y, cor, .size = 2), c(NA, -1))
  expect_equal(stretch2(x, y, cor, .size = 2, .init = 2), c(-1, -1))
})

y <- 3:1

test_that("slide2()", {
  expect_equal(slide2(x, y, sum), rep(4, 3))
  expect_equal(slide2(x, y, cor, .size = 2), c(NA, -1, -1))
  expect_equal(slide2(x, y, cor, .size = 2, .fill = 0), c(0, -1, -1))
})

test_that("tile2()", {
  expect_equal(tile2(x, y, sum), rep(4, 3))
  expect_equal(tile2(x, y, sum, .size = 2), c(8, 4))
  expect_equal(tile2(x, y, cor, .size = 2), c(-1, NA))
})

test_that("stretch2()", {
  expect_equal(stretch2(x, y, sum), c(4, 8, 12))
  expect_equal(stretch2(x, y, cor, .size = 2), c(NA, -1))
  expect_equal(stretch2(x, y, cor, .size = 2, .init = 2), c(-1, -1))
})

test_that("slide2_lst()", {
  expect_equal(slide2_lst(x, y, sum), list(4, 4, 4))
  expect_equal(slide2_lst(x, y, cor, .size = 2), list(NA, -1, -1))
  expect_equal(slide2_lst(x, y, cor, .size = 2, .fill = 0), list(0, -1, -1))
})

test_that("tile2_lst()", {
  expect_equal(tile2_lst(x, y, sum), list(4, 4, 4))
  expect_equal(tile2_lst(x, y, sum, .size = 2), list(8, 4))
  expect_equal(tile2_lst(x, y, cor, .size = 2), list(-1, NA_real_))
})

test_that("stretch2_lst()", {
  expect_equal(stretch2_lst(x, y, sum), list(4, 8, 12))
  expect_equal(stretch2_lst(x, y, cor, .size = 2), list(NA_real_, -1))
  expect_equal(stretch2_lst(x, y, cor, .size = 2, .init = 2), list(-1, -1))
})

z <- 3:1

test_that("pslide()", {
  expect_equal(pslide(list(x, y, z), sum), 7:5)
  expect_equal(pslide(list(x, y, z), sum, .size = 2), c(NA, 13, 11))
  expect_equal(pslide(list(x, y, z), sum, .size = 2, .fill = 0), c(0, 13, 11))
})

test_that("ptile()", {
  expect_equal(ptile(list(x, y, z), sum), 7:5)
  expect_equal(ptile(list(x, y, z), sum, .size = 2), c(13, 5))
})

test_that("pstretch()", {
  expect_equal(pstretch(list(x, y, z), sum), c(7, 13, 18))
  expect_equal(pstretch(list(x, y, z), sum, .size = 2), c(7, 18))
  expect_equal(pstretch(list(x, y, z), sum, .size = 2, .init = 2), c(13, 18))
})

test_that("pslide_lst()", {
  expect_equal(pslide_lst(list(x, y, z), sum), list(7, 6, 5))
  expect_equal(pslide_lst(list(x, y, z), sum, .size = 2), list(NA, 13, 11))
  expect_equal(pslide_lst(list(x, y, z), sum, .size = 2, .fill = 0), list(0, 13, 11))
})

test_that("ptile_lst()", {
  expect_equal(ptile_lst(list(x, y, z), sum), list(7, 6, 5))
  expect_equal(ptile_lst(list(x, y, z), sum, .size = 2), list(13, 5))
})

test_that("pstretch_lst()", {
  expect_equal(pstretch_lst(list(x, y, z), sum), list(7, 13, 18))
  expect_equal(pstretch_lst(list(x, y, z), sum, .size = 2), list(7, 18))
  expect_equal(pstretch_lst(list(x, y, z), sum, .size = 2, .init = 2), list(13, 18))
})

sx <- pedestrian %>%
  filter(Sensor == "Southern Cross Station", Date <= as.Date("2015-01-06"))

test_that("slide_*()", {
  qtl_df <- sx %>%
    slide_dfr(~ quantile(.$Count), .size = 24)
  expect_equal(dim(qtl_df), c(NROW(sx), 5))
  expect_is(qtl_df, "tbl_df")
  qtl_lst <- sx %>%
    slide_lst(~ quantile(.$Count), .size = 24)
  expect_equal(NROW(qtl_df), NROW(sx))
  expect_is(qtl_lst, "list")
})

test_that("tile_*()", {
  qtl_df <- sx %>%
    tile_dfr(~ quantile(.$Count), .size = 24)
  expect_equal(dim(qtl_df), c(NROW(sx) / 24, 5))
  expect_is(qtl_df, "tbl_df")
  qtl_lst <- sx %>%
    tile_lst(~ quantile(.$Count), .size = 24)
  expect_equal(NROW(qtl_df), NROW(sx) / 24)
  expect_is(qtl_lst, "list")
})

test_that("stretch_*()", {
  qtl_df <- sx %>%
    stretch_dfr(~ quantile(.$Count), .init = 48)
  expect_equal(dim(qtl_df), c(NROW(sx) - 47, 5))
  expect_is(qtl_df, "tbl_df")
  qtl_lst <- sx %>%
    stretch_lst(~ quantile(.$Count), .init = 48)
  expect_equal(NROW(qtl_df), NROW(sx) - 47)
  expect_is(qtl_lst, "list")
})

