context("Test pillar methods")
library(pillar)

x <- yearmonth(seq(2010, 2012, by = 1 / 12))
x2 <- rep(x, 2)
y <- yearquarter(seq(2010, 2012, by = 1 / 4))
y2 <- rep(y, 2)

test_that("Test some S3 methods for yearmonth & yearquarter", {
  expect_is(rep(x, 2), "yearmonth")
  expect_equal(length(rep(x, 2)), length(x) * 2)
  expect_is(c(x, x), "yearmonth")
  expect_is(unique(x2), "yearmonth")
  expect_identical(yearmonth(x), x)
  expect_is(rep(y, 2), "yearquarter")
  expect_equal(length(rep(y, 2)), length(y) * 2)
  expect_is(c(y, y), "yearquarter")
  expect_is(unique(y2), "yearquarter")
  expect_identical(yearquarter(y), y)
  expect_is(y[1:2], "yearquarter")
})

test_that("Test pillar methods", {
  expect_equal(type_sum(x), "mth")
  expect_equal(is_vector_s3(x), TRUE)
  expect_equal(type_sum(y), "qtr")
  expect_equal(is_vector_s3(y), TRUE)
  expect_equal(obj_sum(x), rep("mth", length(x)))
  expect_equal(obj_sum(y), rep("qtr", length(y)))

  expect_equal(type_sum(pedestrian), "tsibble")
})

tbl1 <- tsibble(
  date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
  value = rnorm(10),
  key = id(), index = date
)
tbl2 <- tsibble(
  qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30),
  key = id(group), index = qtr
)

test_that("Test tbl_sum()", {
  expect_identical(tbl_sum(tbl1), c("A tsibble" = "10 x 2 [1DAY]"))
  expect_identical(
    tbl_sum(tbl2), 
    c("A tsibble" = "30 x 3 [1QUARTER]", "Keys" = "group [3]"))
})
