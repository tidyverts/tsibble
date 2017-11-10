context("Test as_tsibble() w/o data of wide form")

test_that("Test a ts with different frequnecy", {
  x1 <- ts(1:10)
  tsbl1 <- as_tsibble(x1)
  expect_identical(dim(tsbl1), c(length(x1), 2L))
  expect_identical(key_vars(tsbl1), "NULL")
  expect_identical(format(interval(tsbl1)), "1UNIT")
  x2 <- ts(1:10, start = 2000)
  tsbl2 <- as_tsibble(x2)
  expect_identical(format(interval(tsbl2)), "1YEAR")
  x3 <- ts(1:10, start = c(2000, 1), frequency = 4)
  tsbl3 <- as_tsibble(x3)
  expect_identical(format(interval(tsbl3)), "1QUARTER")
  x4 <- ts(1:10, start = c(2000, 1), frequency = 12)
  tsbl4 <- as_tsibble(x4)
  expect_identical(format(interval(tsbl4)), "1MONTH")
})

test_that("Test a mts", {
  x <- ts(matrix(1:10, ncol = 2))
  tsbl1 <- as_tsibble(x)
  expect_identical(dim(tsbl1), c(length(x), 3L))
  expect_identical(key_vars(tsbl1)[[1L]], "key")
  tsbl2 <- as_tsibble(x, gather = FALSE)
  expect_identical(dim(tsbl2), c(nrow(x), 3L))
  expect_identical(key_vars(tsbl2), "NULL")
  expect_identical(colnames(tsbl2), c("index", "Series 1", "Series 2"))
})

test_that("Test a hts", {
  eg1 <- hts::htseg1
  tsbl1 <- as_tsibble(hts::htseg1)
  expect_identical(dim(tsbl1), c(nrow(eg1$bts) * ncol(eg1$bts), 4L))
  expect_identical(
    key_vars(tsbl1), 
    c("`Level 2` | `Level 1`" = "`Level 2` | `Level 1`")
  )
  expect_identical(unique(tsbl1$`Level 2`), unname(eg1$labels[[3]]))
  expect_identical(tsbl1$`Level 2`, rep(eg1$labels[[3]], each = 10))
  expect_identical(unique(tsbl1$`Level 1`), unname(eg1$labels[[2]]))
  expect_identical(tsbl1$`Level 1`, rep(eg1$labels[[2]], times = c(30, 20)))
})
