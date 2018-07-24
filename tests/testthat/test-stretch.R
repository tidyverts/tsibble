context("Stretching window function and its variants")

x <- 1:5
y <- 6:10
z <- 11:15
lst <- list(x = x, y = y, z = z)
df <- as.data.frame(lst)
lst_cols <- tibble::tibble(lst = list(df, df, df))

test_that("stretcher() & pstretcher()", {
  expect_error(stretcher(x, .init = -1, .size = 2), "positive")
  expect_equal(stretcher(x, .size = 2), list(1, 1:3, 1:5))
  expect_equal(stretcher(x, .size = -2), list(5, 5:3, 5:1))
  expect_equal(
    stretcher(lst, .size = 2),
    list(list(x = x), list(x = x, y = y, z = z))
  )
  expect_equal(
    stretcher(list(x, y), .size = 2),
    list(list(x), list(x, y))
  )
  expect_equal(
    stretcher(df, .combine = TRUE),
    list(df[1, , drop = FALSE], df[1:2, ], df[1:3, ], df[1:4, ], df)
  )
  expect_equal(
    stretcher(lst_cols$lst, .combine = TRUE),
    list(lst_cols$lst[[1]], dplyr::bind_rows(lst_cols$lst[1:2]), dplyr::bind_rows(lst_cols$lst[1:3]))
  )
  expect_equal(
    pstretcher(lst, .size = 2),
    list(list(list(x = x), list(x = x, y = y, z = z)))
  )
  expect_equal(
    pstretcher(list(x, y), list(y, y), .size = 1),
    list(list(list(x), list(x, y)), list(list(y), list(y, y)))
  )
  expect_equal(
    pstretcher(df, .size = 2),
    pstretcher(lst, .size = 2)
  )
  expect_equal(
    pstretcher(df, df, .size = 2),
    pstretcher(lst, lst, .size = 2)
  )
})

test_that("stretch() and its variants", {
  expect_equal(
    stretch_dbl(x, mean, .size = 2),
    purrr::map_dbl(stretcher(x, 2), mean)
  )
  expect_equal(
    stretch(lst, ~ ., .size = 2),
    list(list(x = x), list(x = x, y = y, z = z))
  )
  expect_equal(
    stretch_dfr(x, ~ data.frame(x = mean(.)), .size = 1),
    data.frame(x = purrr::map_dbl(stretcher(x, 1), mean))
  )
  expect_equal(
    stretch_dfc(x, ~ data.frame(x = mean(.)), .size = 1),
    data.frame(x = 1, x1 = 1.5, x2 = 2, x3 = 2.5, x4 = 3)
  )
  expect_equal(
    stretch_dfr(x, quantile, 0.5, .size = 2),
    tibble::tibble(`50%` = stretch_dbl(x, quantile, 0.5, .size = 2))
  )
})

test_that("stretch2() and its variants", {
  expect_equal(
    stretch2_int(x, y, sum, .size = 2),
    c(7L, 27L, 55L)
  )
  expect_equal(
    stretch2(lst, lst, ~ ., .size = 2),
    list(list(x = x), list(x = x, y = y, z = z))
  )
  expect_equal(
    stretch2(df, df, ~ ., .size = 2),
    stretch2(lst, lst, ~ ., .size = 2)
  )
  expect_equal(
    stretch2_dfr(df, df, ~ tibble::tibble(x = sum(.x, .y)), .size = 2, .combine = TRUE)[1, 1, drop  = TRUE],
    sum(df[1, ]) * 2
  )
  expect_equal(
    stretch2_dfc(df, df, ~ tibble::tibble(x = sum(.x, .y)), .size = 2, .combine = TRUE)[1, 1, drop  = TRUE],
    sum(df[1, ]) * 2
  )
})

test_that("pstretch() and its variants", {
  expect_equal(
    pstretch_lgl(lst, ~ sum(..1, ..2) > 10, size = 1),
    pstretch_int(lst, ~ sum(..1, ..2), size = 1) > 10
  )
  expect_equal(
    pstretch(list(lst, lst), ~ ..1, .size = 2),
    list(list(x = x), list(x = x, y = y, z = z))
  )
  my_sum2 <- function(...) {
    data <- list(...)
    data.frame(x = sum(data$x))
  }
  expect_equal(
    pstretch_dfr(df, my_sum2, .size = 2),
    data.frame(x = c(1L, 6L, 15L))
  )
  expect_equal(
    pstretch_dfc(df, my_sum2, .size = 2),
    data.frame(x = 1L, x1 = 6L, x2 = 15L)
  )
})
