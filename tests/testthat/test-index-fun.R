context("year-week, year-month, year-quarter")

yw <- seq(yearweek(as.Date("1970-01-01")), length.out = 3, by = 1)
ym <- seq(yearmonth(1970 + 0 / 12), length.out = 3, by = 1)
yq <- seq(yearquarter(1970 + 0 / 4), length.out = 3, by = 1)

test_that("diff()", {
  expect_is(diff(yw), "difftime")
  expect_equal(as.numeric(diff(yw)), rep(1, 2))
  expect_equal(as.numeric(diff(ym)), rep(1, 2))
  expect_equal(as.numeric(diff(yq)), rep(1, 2))
})

a <- yearweek(seq(ymd("2017-02-01"), length.out = 12, by = "1 week"))
a2 <- rep(a, 2)
x <- yearmonth(seq(2010, 2012, by = 1 / 12))
x2 <- rep(x, 2)
y <- yearquarter(seq(2010, 2012, by = 1 / 4))
y2 <- rep(y, 2)

test_that("some S3 methods for yearweek, yearmonth & yearquarter", {
  expect_is(rep(a, 2), "yearweek")
  expect_equal(length(rep(a, 2)), length(a) * 2)
  expect_is(c(a, a), "yearweek")
  expect_is(unique(a2), "yearweek")
  expect_identical(yearweek(a), a)
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

test_that("unsupported class for index functions", {
  expect_error(yearweek(seq(2010, 2012, by = 1 / 52)), "coerce the `numeric`")
})

xx <- make_datetime(2018, 1, 1, 0)

test_that("POSIXct", {
  expect_equal(format(yearweek(xx)), "2018 W01")
  expect_equal(format(yearmonth(xx)), "2018 Jan")
  expect_equal(format(yearquarter(xx)), "2018 Q1")
})

test_that("character", {
  expect_equal(format(yearweek(as.character(xx))), "2018 W01")
  expect_equal(format(yearmonth(as.character(xx))), "2018 Jan")
  expect_equal(format(yearquarter(as.character(xx))), "2018 Q1")
})
