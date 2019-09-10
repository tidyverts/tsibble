context("year-week, year-month, year-quarter")

yw <- seq(yearweek(as.Date("1970-01-01")), length.out = 3, by = 1)
ym <- seq(yearmonth(1970 + 0 / 12), length.out = 3, by = 1)
yq <- seq(yearquarter(1970 + 0 / 4), length.out = 3, by = 1)

test_that("subset by 0", {
  expect_is(yw[0], "yearweek")
  expect_is(ym[0], "yearmonth")
  expect_is(yq[0], "yearquarter")
})

test_that("is_53weeks()", {
  expect_equal(is_53weeks(2015:2016), c(TRUE, FALSE))
  expect_error(is_53weeks("2015"), "positive integers.")
})

test_that("units_since()", {
  expect_equal(units_since(yw), 0:2)
  expect_equal(units_since(ym), 0:2)
  expect_equal(units_since(yq), 0:2)
  expect_equal(units_since(year(yq)), rep(0, 3))
  expect_equal(units_since(1:3), 1:3)
})

test_that("diff()", {
  expect_is(diff(yw), "difftime")
  expect_equal(as.numeric(diff(yw)), rep(1, 2))
  expect_identical(interval_pull(diff(yw) + 0:1), new_interval(week = 1))
  expect_equal(as.numeric(diff(ym)), rep(1, 2))
  expect_identical(interval_pull(diff(ym) + c(2, 4)), new_interval(month = 2))
  expect_equal(as.numeric(diff(yq)), rep(1, 2))
  expect_identical(interval_pull(diff(yq) + c(3, 6)), new_interval(quarter = 3))
})

test_that("Arithmetic", {
  expect_equal(format(+yw), format(yw))
  expect_equal(format(yw + 1), c("1970 W02", "1970 W03", "1970 W04"))
  expect_equal(format(1 + yw), format(yw + 1))
  expect_equal(format(yw - 1), c("1969 W52", "1970 W01", "1970 W02"))
  expect_error(yw + yw, "not defined")
  expect_equal(as.integer(yw - yw), rep(0, 3))
  expect_equal(as.integer(yw[2] - yw[1]), 1)
  expect_equal(as.integer(yw[1] - yw[2]), -1)
  expect_equal(format(+ym), format(ym))
  expect_equal(format(ym + 1), c("1970 Feb", "1970 Mar", "1970 Apr"))
  expect_equal(format(1 + ym), format(ym + 1))
  expect_equal(format(ym - 1), c("1969 Dec", "1970 Jan", "1970 Feb"))
  expect_equal(format(+yq), format(yq))
  expect_equal(format(yq + 1), c("1970 Q2", "1970 Q3", "1970 Q4"))
  expect_equal(format(1 + yq), format(yq + 1))
  expect_equal(format(yq - 1), c("1969 Q4", "1970 Q1", "1970 Q2"))
  expect_error(yq + yq, "not defined")
  expect_equal(as.integer(yq - yq), rep(0, 3))
  expect_equal(as.integer(yq[2] - yq[1]), 1)
  expect_equal(as.integer(yq[1] - yq[2]), -1)
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
  expect_error(yearweek(seq(2010, 2012, by = 1 / 52)), "handle the numeric")
})

xx <- make_datetime(2018, 1, 1, 0)

test_that("POSIXct", {
  expect_equal(format(yearweek(xx)), "2018 W01")
  expect_equal(format(yearmonth(xx)), "2018 Jan")
  expect_equal(format(yearquarter(xx)), "2018 Q1")
})

test_that("character", {
  skip_on_os("solaris")
  expect_equal(format(yearweek(as.character(xx))), "2018 W01")
  expect_equal(format(yearmonth(as.character(xx))), "2018 Jan")
  expect_equal(format(yearmonth("201801")), "2018 Jan")
  expect_equal(format(yearquarter(as.character(xx))), "2018 Q1")
})

test_that("yearmonth() #89", {
  expect_false(
    anyNA(yearmonth(as.numeric(time(
      ts(rnorm(139), frequency = 12, start = c(1978, 2))
    ))))
  )
})

test_that("yearquarter() for characters #107", {
  expect_error(yearquarter("2013 Qt 3"), "cannot be expressed as Date type")
  expect_error(yearquarter("Qtrr 5 2015"), "cannot be expressed as Date type")
  expect_error(yearquarter("Quar 5 2015"), "cannot be expressed as Date type")
  expect_error(yearquarter("Q5 2015"), "can't be greater than 4.")
  expect_error(yearquarter("Q2015"), "unambiguous")
  expect_error(yearquarter(c("Q2015", "Q2 2015")), "unambiguous")
  expect_identical(
    yearquarter(c("2013 Q3", "2013 Qtr 3", "Quarter 3 2013")),
    rep(yearquarter("2013 Q3"), 3)
  )
})

test_that("yearquarter.character() underlying dates #129", {
  expect_equal(as.Date(yearquarter("2017 Q1")), as.Date("2017-01-01"))
})

test_that("yearweek() for characters", {
  expect_error(yearweek("2013 We 3"), "cannot be expressed as Date type")
  expect_error(yearweek("Wee 5 2015"), "cannot be expressed as Date type")
  expect_error(yearweek("W54 2015"), "can't be greater than 53.")
  expect_error(yearweek(c("2015 W53", "2016 W53", "2017 W53")), "can't be 53 weeks.")
  expect_error(yearweek("W2015"), "unambiguous")
  expect_error(yearweek(c("W2015", "W2 2015")), "unambiguous")
  expect_identical(
    yearweek(c("2013 W3", "2013 Wk 3", "Week 3 2013")),
    rep(yearweek("2013 W03"), 3)
  )
})

test_that("yearweek.character() underlying dates", {
  expect_equal(as.Date(yearweek("1970 W01")), as.Date("1969-12-29"))
  expect_equal(as.Date(yearweek("2019 W12")), as.Date("2019-03-18"))
})
