context("Rolling window function and its variants")

x <- 1:3
xx <- list(a = rep(1L, 3), b = rep(TRUE, 3), c = rep("a", 3))

test_that("slide() and slider() output", {
  expect_error(slider(x, .size = 0))
  expect_error(slider(list(list(x))))
  expect_error(slide(xx), "atomic vector")
  expect_equal(slider(x), list(1, 2, 3))
  expect_equal(slider(x, .size = 2), list(1:2, 2:3))
  expect_equal(slide_dbl(x, sum), 1:3)
  expect_equal(slide_int(x, sum), 1L:3L)
  expect_equal(slide_lgl(x, ~ sum(.) > 1L), c(FALSE, TRUE, TRUE))
  expect_equal(slide_dbl(x, sum, .size = 2), c(NA, 3, 5))
  expect_equal(slide_dbl(x, sum, .size = 2, .fill = 0), c(0, 3, 5))
  expect_equal(slide(x, sum), list(1, 2, 3))
  expect_equal(
    slide_dfr(x, quantile, 0.5),
    tibble::tibble(`50%` = as.numeric(1:3))
  )
  expect_equal(
    slide_dfc(x, ~ data.frame(a = .)),
    data.frame(a = 1, a1 = 2, a2 = 3)
  )
})

test_that("lslide_if() & lslide_at()", {
  expect_error(lslide(x, sum), "accepts a list")
  expect_equal(
    lslide(xx[1:2], sum, .size = 2),
    list(a = c(NA, 2, 2), b = c(NA, 2, 2))
  )
  expect_equal(
    lslide_if(xx, is.integer, sum, .size = 2), 
    list(a = c(NA, 2, 2), b = xx$b, c = xx$c)
  )
  expect_equal(
    lslide_if(xx, is.integer, sum, .size = 2), 
    lslide_if(tibble::as_tibble(xx), is.integer, sum, .size = 2)
  )
  expect_equal(
    lslide_at(xx, "b", sum, .size = 2), 
    list(a = xx$a, b = c(NA, 2, 2), c = xx$c)
  )
})

test_that("tile() and tiler() output", {
  expect_equal(tiler(x), list(1, 2, 3))
  expect_equal(tiler(x, .size = 2), list(1:2, 3))
  expect_equal(tile_dbl(x, sum), 1:3)
  expect_equal(tile_int(x, sum), 1L:3L)
  expect_equal(tile_dbl(x, sum, .size = 2), c(3, 3))
})

test_that("ltile_if() & ltile_at()", {
  expect_equal(
    ltile(xx[1:2], sum, .size = 2),
    list(a = c(2, 1), b = c(2, 1))
  )
  expect_equal(
    ltile_if(xx, is.integer, sum, .size = 2), 
    list(a = c(2, 1), b = xx$b, c = xx$c)
  )
  expect_equal(
    ltile_at(xx, "b", sum, .size = 2), 
    list(a = xx$a, b = c(2, 1), c = xx$c)
  )
})

test_that("stretch() and stretcher() output", {
  expect_error(stretcher(x, .init = c(3, 5)))
  expect_equal(stretcher(x), list(1, 1:2, 1:3))
  expect_equal(stretcher(x, .init = 2), list(1:2, 1:3))
  expect_equal(stretch_dbl(x, sum), c(1, 3, 6))
  expect_equal(stretch_int(x, sum), c(1L, 3L, 6L))
  expect_equal(stretch_dbl(x, sum, .init = 2), c(3, 6))
})

test_that("lstretch_if() & lstretch_at()", {
  expect_equal(
    lstretch(xx[1:2], sum, .size = 2),
    list(a = c(1, 3), b = c(1, 3))
  )
  expect_equal(
    lstretch_if(xx, is.integer, sum, .size = 2), 
    list(a = c(1, 3), b = xx$b, c = xx$c)
  )
  expect_equal(
    lstretch_at(xx, "b", sum, .size = 2), 
    list(a = xx$a, b = c(1, 3), c = xx$c)
  )
})

y <- 3:1

test_that("slide2()", {
  expect_equal(
    slide2_dfr(x, y, ~ data.frame(a = sum(.))),
    data.frame(a = 1:3)
  )
  expect_equal(
    slide2_dfc(x, y, ~ data.frame(a = sum(.))),
    data.frame(a = 1, a1 = 2, a2 = 3)
  )
  expect_equal(slide2_dbl(x, y, sum), rep(4, 3))
  expect_equal(slide2_dbl(x, y, cor, .size = 2), c(NA, -1, -1))
  expect_equal(slide2_dbl(x, y, cor, .size = 2, .fill = 0), c(0, -1, -1))
})

test_that("tile2()", {
  expect_equal(tile2_dbl(x, y, sum), rep(4, 3))
  expect_equal(tile2_dbl(x, y, sum, .size = 2), c(8, 4))
  expect_equal(tile2_dbl(x, y, cor, .size = 2), c(-1, NA))
})

test_that("stretch2()", {
  expect_equal(stretch2_dbl(x, y, sum), c(4, 8, 12))
  expect_equal(stretch2_dbl(x, y, cor, .size = 2), c(NA, -1))
  expect_equal(stretch2_dbl(x, y, cor, .size = 2, .init = 2), c(-1, -1))
})

y <- 3:1

test_that("slide2_dbl()", {
  expect_equal(slide2_dbl(x, y, sum), rep(4, 3))
  expect_equal(slide2_dbl(x, y, cor, .size = 2), c(NA, -1, -1))
  expect_equal(slide2_dbl(x, y, cor, .size = 2, .fill = 0), c(0, -1, -1))
})

test_that("tile2_dbl()", {
  expect_equal(tile2_dbl(x, y, sum), rep(4, 3))
  expect_equal(tile2_dbl(x, y, sum, .size = 2), c(8, 4))
  expect_equal(tile2_dbl(x, y, cor, .size = 2), c(-1, NA))
})

test_that("stretch2_dbl()", {
  expect_equal(stretch2_dbl(x, y, sum), c(4, 8, 12))
  expect_equal(stretch2_dbl(x, y, cor, .size = 2), c(NA, -1))
  expect_equal(stretch2_dbl(x, y, cor, .size = 2, .init = 2), c(-1, -1))
})

test_that("slide2_dbl()", {
  expect_equal(slide2(x, y, sum), list(4, 4, 4))
  expect_equal(slide2(x, y, cor, .size = 2), list(NA, -1, -1))
  expect_equal(slide2(x, y, cor, .size = 2, .fill = 0), list(0, -1, -1))
})

test_that("tile2()", {
  expect_equal(tile2(x, y, sum), list(4, 4, 4))
  expect_equal(tile2(x, y, sum, .size = 2), list(8, 4))
  expect_equal(tile2(x, y, cor, .size = 2), list(-1, NA_real_))
})

test_that("stretch2()", {
  expect_equal(stretch2(x, y, sum), list(4, 8, 12))
  expect_equal(stretch2(x, y, cor, .size = 2), list(NA_real_, -1))
  expect_equal(stretch2(x, y, cor, .size = 2, .init = 2), list(-1, -1))
})

z <- 3:1

test_that("pslide_dbl()", {
  expect_equal(pslide_dbl(list(x, y, z), sum), 7:5)
  expect_equal(pslide_dbl(list(x, y, z), sum, .size = 2), c(NA, 13, 11))
  expect_equal(pslide_dbl(list(x, y, z), sum, .size = 2, .fill = 0), c(0, 13, 11))
})

test_that("ptile_dbl()", {
  expect_equal(ptile_dbl(list(x, y, z), sum), 7:5)
  expect_equal(ptile_dbl(list(x, y, z), sum, .size = 2), c(13, 5))
})

test_that("pstretch_dbl()", {
  expect_equal(pstretch_dbl(list(x, y, z), sum), c(7, 13, 18))
  expect_equal(pstretch_dbl(list(x, y, z), sum, .size = 2), c(7, 18))
  expect_equal(pstretch_dbl(list(x, y, z), sum, .size = 2, .init = 2), c(13, 18))
})

test_that("pslide()", {
  expect_equal(pslide(list(x, y, z), sum), list(7, 6, 5))
  expect_equal(pslide(list(x, y, z), sum, .size = 2), list(NA, 13, 11))
  expect_equal(pslide(list(x, y, z), sum, .size = 2, .fill = 0), list(0, 13, 11))
})

test_that("ptile()", {
  expect_equal(ptile(list(x, y, z), sum), list(7, 6, 5))
  expect_equal(ptile(list(x, y, z), sum, .size = 2), list(13, 5))
})

test_that("pstretch()", {
  expect_equal(pstretch(list(x, y, z), sum), list(7, 13, 18))
  expect_equal(pstretch(list(x, y, z), sum, .size = 2), list(7, 18))
  expect_equal(pstretch(list(x, y, z), sum, .size = 2, .init = 2), list(13, 18))
})
