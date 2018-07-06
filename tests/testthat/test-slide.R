context("Sliding window function and its variants")

.x <- 1:5
.y <- 6:10
.z <- 11:15
.lst <- list(x = .x, y = .y, z = .z)
.df <- as.data.frame(.lst)

test_that("Invalid input", {
  expect_error(slider(.lst, 0), "must not be 0.")
  expect_error(slider(.lst, c(1, 3)), "must be an integer.")
  expect_error(slider(.x, -6), "larger")
  expect_error(slider(list()), "larger")
})

test_that("slider() & pslider()", {
  expect_equal(slider(.x, .size = 2), list(1:2, 2:3, 3:4, 4:5))
  expect_equal(slider(.x, .size = -2), list(5:4, 4:3, 3:2, 2:1))
  expect_equal(slider(.x, .size = 2, .partial = TRUE), list(c(NA, 1), 1:2, 2:3, 3:4, 4:5))
  expect_equal(
    slider(.lst, .size = 2),
    list(list(x = .x, y = .y), list(y = .y, z = .z))
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
    list(list(list(.x, .y)), list(list(.y, .y)))
  )
  expect_equal(
    pslider(.df, .size = 2),
    pslider(.lst, .size = 2)
  )
  expect_equal(
    pslider(.df, .df, .size = 2),
    pslider(.lst, .lst, .size = 2)
  )
  expect_equal(
    pslider(.x, 1), # recycling
    list(as.list(1:5), as.list(rep(1, 5)))
  )
})

test_that("slide() and its variants", {
  expect_equal(
    slide_dbl(.x, mean, .size = 2),
    c(NA, purrr::map_dbl(slider(.x, 2), mean))
  )
  expect_equal(
    slide(.lst, ~ ., .size = 2, .partial = TRUE),
    list(list(NA, x = .x), list(x = .x, y = .y), list(y = .y, z = .z))
  )
  expect_equal(
    slide(.lst, ~ ., .size = 2),
    list(NA, list(x = .x, y = .y), list(y = .y, z = .z))
  )
  expect_equal(
    slide_dfr(.x, ~ data.frame(x = .), .size = 1),
    data.frame(x = .x)
  )
  # expect_equal(
  #   slide_dfc(.x, ~ data.frame(x = .), .size = 1),
  #   data.frame(x = 1, x = 2, x = 3, x = 4, x = 5)
  # )
})

test_that("slide2() and its variants", {
  expect_equal(
    slide2_int(.x, .y, sum, .size = 2, .fill = NA_integer_),
    c(NA_integer_, seq.int(16, by = 4, length.out = 4))
  )
  expect_equal(
    slide2(.lst, .lst, ~ sum(unlist(.x), unlist(.y)), .size = 2, .fill = NA_real_),
    list(NA_real_, 110, 210)
  )
  expect_equal(
    slide2(.df, .df, ~ ., .size = 2),
    slide2(.lst, .lst, ~ ., .size = 2)
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

x1 <- list(1:3, 1, 1:2)
x2 <- list(1:3, 1)
x3 <- list(1:3, 1:3)

test_that("recycle()", {
  expect_error(recycle(x1), "Element 3 has length 2, not 1 or 3.")
  expect_equal(recycle(x2), list(1:3, rep(1, 3)))
  expect_equal(recycle(x3), x3)
})
