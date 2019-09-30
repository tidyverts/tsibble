context("year-week, year-month, year-quarter")

yw <- seq(yearweek(as.Date("1970-01-01")), length.out = 3, by = 1)
ym <- seq(yearmonth(1970 + 0 / 12), length.out = 3, by = 1)
yq <- seq(yearquarter(1970 + 0 / 4), length.out = 3, by = 1)

test_that("units_since()", {
  expect_equal(units_since(yw), 0:2)
  expect_equal(units_since(ym), 0:2)
  expect_equal(units_since(yq), 0:2)
  expect_equal(units_since(year(yq)), rep(0, 3))
  expect_equal(units_since(1:3), 1:3)
})
