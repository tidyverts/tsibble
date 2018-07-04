context("Sliding window function and its variants")

.x <- 1:5
.y <- 6:10
.z <- 11:15
.lst <- list(x = .x, y = .y, z = .z)
.df <- as.data.frame(.lst)

test_that("Invalid input", {
  expect_error(slider(list(list(.lst))), "must not be deeper than 3.")
  expect_error(slider(.lst, 0), "a positive integer.")
})

test_that("slider() & pslider()", {
  expect_equal(slider(.x, .size = 2), list(1:2, 2:3, 3:4, 4:5))
  expect_equal(
    slider(.lst, .size = 2),
    list(list(x = .x, y = .y), list(y = .y, z = .z))
  )
  expect_equal(
    slider(list(.x, .y), .size = 2),
    list(list(.x, .y))
  )
  expect_equal(
    slider(.df, .size = 2),
    slider(.lst, .size = 2)
  )
  expect_equal(
    pslider(.lst, .size = 2),
    list(list(list(x = .x, y = .y), list(y = .y, z = .z)))
  )
  expect_equal(
    pslider(list(.x, .y), list(.y), .size = 2),
    list(list(list(.x, .y)), list())
  )
  expect_equal(
    pslider(.df, .size = 2),
    pslider(.lst, .size = 2)
  )
  expect_equal(
    pslider(.df, .df, .size = 2),
    pslider(.lst, .lst, .size = 2)
  )
})

test_that("slide() and its variants", {
  expect_equal(
    slide_dbl(.x, mean, .size = 2),
    c(NA, purrr::map_dbl(slider(.x, 2), mean))
  )
  expect_equal(
    slide(.lst, ~ ., .size = 2),
    list(NA, c(.x, .y), c(.y, .z))
  )
  expect_equal(
    slide_dfr(.x, ~ data.frame(x = .), .size = 1),
    data.frame(x = .x)
  )
  expect_equal(
    slide_dfc(.x, ~ data.frame(x = .), .size = 1),
    data.frame(x = 1, x1 = 2, x2 = 3, x3 = 4, x4 = 5)
  )
  expect_equal(
    slide_dfr(.x, quantile, 0.5, .size = 2),
    tibble::tibble(`50%` = slide_dbl(.x, quantile, 0.5, .size = 2))
  )
})

test_that("slide2() and its variants", {
  expect_equal(
    slide2_int(.x, .y, sum, .size = 2),
    c(NA, seq.int(16, by = 4, length.out = 4))
  )
  expect_equal(
    slide2(.lst, .lst, sum, .size = 2),
    list(NA, 110, 210)
  )
  expect_equal(
    slide2(.df, .df, sum, .size = 2),
    slide2(.lst, .lst, sum, .size = 2)
  )
})

test_that("pslide() and its variants", {
  expect_equal(
    pslide_lgl(.lst, ~ sum(..1, ..2) > 10, size = 1),
    pslide_int(.lst, ~ sum(..1, ..2), size = 1) > 10
  )
  expect_equal(
    pslide(.lst, ~ sum(..1, ..2) > 10, size = 1),
    as.list(pslide_int(.lst, ~ sum(..1, ..2), size = 1) > 10)
  )
  expect_equal(
    pslide(list(.lst, .lst), ~ ..1, .size = 2),
    list(NA, list(x = .x, y = .y), list(y = .y, z = .z))
  )
  expect_equal(
    pslide(list(list(.x, .y), list(.y)), ~ list(..1, ..2)),
    pslide(list(list(.x, .y), list(.y, .y)), ~ list(..1, ..2))
  )
})
