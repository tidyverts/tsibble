context("Stretching window function and its variants")

.x <- 1:5
.y <- 6:10
.z <- 11:15
.lst <- list(x = .x, y = .y, z = .z)
.df <- as.data.frame(.lst)

test_that("stretcher() & pstretcher()", {
  expect_equal(stretcher(.x, .size = 2), list(1, 1:3, 1:5))
  expect_equal(
    stretcher(.lst, .size = 2),
    list(list(x = .x), list(x = .x, y = .y, z = .z))
  )
  expect_equal(
    stretcher(list(.x, .y), .size = 2),
    list(list(.x), list(.x, .y))
  )
  expect_equal(
    pstretcher(.lst, .size = 2),
    list(list(list(x = .x), list(x = .x, y = .y, z = .z)))
  )
  expect_equal(
    pstretcher(list(.x, .y), list(.y, .y), .size = 1),
    list(list(list(.x), list(.x, .y)), list(list(.y), list(.y, .y)))
  )
  expect_equal(
    pstretcher(.df, .size = 2),
    pstretcher(.lst, .size = 2)
  )
  expect_equal(
    pstretcher(.df, .df, .size = 2),
    pstretcher(.lst, .lst, .size = 2)
  )
})

test_that("stretch() and its variants", {
  expect_equal(
    stretch_dbl(.x, mean, .size = 2),
    purrr::map_dbl(stretcher(.x, 2), mean)
  )
  expect_equal(
    stretch(.lst, ~ ., .size = 2),
    list(c(.x), c(.x, .y, .z))
  )
  expect_equal(
    stretch_dfr(.x, ~ data.frame(x = mean(.)), .size = 1),
    data.frame(x = purrr::map_dbl(stretcher(.x, 1), mean))
  )
  expect_equal(
    stretch_dfc(.x, ~ data.frame(x = mean(.)), .size = 1),
    data.frame(x = 1, x1 = 1.5, x2 = 2, x3 = 2.5, x4 = 3)
  )
  expect_equal(
    stretch_dfr(.x, quantile, 0.5, .size = 2),
    tibble::tibble(`50%` = stretch_dbl(.x, quantile, 0.5, .size = 2))
  )
})

test_that("stretch2() and its variants", {
  expect_equal(
    stretch2_int(.x, .y, sum, .size = 2),
    c(7L, 27L, 55L)
  )
  expect_equal(
    stretch2(.lst, .lst, sum, .size = 2),
    list(30, 240)
  )
  expect_equal(
    stretch2(.df, .df, sum, .size = 2),
    stretch2(.lst, .lst, sum, .size = 2)
  )
})

test_that("pstretch() and its variants", {
  expect_equal(
    pstretch_lgl(.lst, ~ sum(..1, ..2) > 10, size = 1),
    pstretch_int(.lst, ~ sum(..1, ..2), size = 1) > 10
  )
  expect_equal(
    pstretch(list(.lst, .lst), ~ ..1, .size = 2),
    list(list(x = .x), list(x = .x, y = .y, z = .z))
  )
})
