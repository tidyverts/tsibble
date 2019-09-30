context("yearweek()")

x <- yearweek(as.Date("1970-01-01")) + 0:2
dates <- seq(as.Date("1969-12-29"), length.out = 3, by = "1 week") + 0
dttm <- as.POSIXct(dates, tz = "UTC")

test_that("is_53weeks()", {
  expect_equal(is_53weeks(2015:2016), c(TRUE, FALSE))
  expect_error(is_53weeks("2015"), "positive integers.")
})

test_that("input types for yearweek()", {
  expect_error(yearweek(seq(2010, 2012, by = 1 / 52)), "handle the numeric")
  expect_identical(yearweek(dttm), x)
  expect_identical(yearweek(dates), x)
  expect_identical(yearweek(x), x)
})

test_that("character type for yearweek()", {
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

test_that("vec_arith() for yearweek()", {
  expect_identical(x + 1:3, yearweek(c("1970 W02", "1970 W04", "1970 W06")))
  expect_identical(x - 1, yearweek(c("1969 W52", "1970 W01", "1970 W02")))
  expect_identical(+ x, x)
  expect_identical(1 + x, x + 1)
  expect_error(x - x, class = "vctrs_error_incompatible_op")
  expect_error(x + x, class = "vctrs_error_incompatible_op")
})

test_that("vec_compare() for yearweek()", {
  expect_identical(x == yearweek("1970 W02"), c(FALSE, TRUE, FALSE))
  expect_identical(x <= yearweek("1970 W02"), c(TRUE, TRUE, FALSE))
  expect_identical(x > yearweek("1970 W02"), c(FALSE, FALSE, TRUE))
  expect_identical(sort(x), x)
})

test_that("vec_cast() for yearweek()", {
  expect_identical(as.Date(x), dates)
  expect_identical(vec_cast(x, to = double()), as.double(x))
  expect_identical(vec_cast(x, to = new_date()), dates)
  # expect_identical(as.POSIXct(x), dttm)
  expect_identical(as.POSIXlt(x), as.POSIXlt(dttm, tz = "UTC"))
  # expect_identical(vec_cast(x, to = new_datetime()), dttm)
})

test_that("vec_c() for yearweek()", {
  expect_identical(vec_c(dates, x), rep(dates, times = 2))
  expect_identical(vec_c(x, dates), rep(dates, times = 2))
  # expect_identical(vec_c(dttm, x), rep(dttm, times = 2))
  # expect_identical(vec_c(x, dttm), rep(dttm, times = 2))
  expect_identical(vec_c(dates, x), c(dates, x))
})
