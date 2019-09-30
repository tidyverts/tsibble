context("yearquarter()")

x <- c("2019 Q3", "2018 Q1")
dates <- as.Date(c("2019-07-01", "2018-01-01"))
dttm <- as.POSIXct(c("2019-07-01", "2018-01-01"), tz = "UTC")

test_that("input types for yearquarter()", {
  expected <- yearquarter(x)
  expect_identical(yearquarter(dttm), expected)
  expect_identical(yearquarter(dates), expected)
  expect_identical(yearquarter(expected), expected)
  expect_identical(yearquarter(x), expected)
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

test_that("vec_arith() for yearquarter()", {
  expect_identical(yearquarter(x) + 1:2, yearquarter(c("2019 Q4", "2018 Q3")))
  expect_identical(yearquarter(x) - 1, yearquarter(c("2019 Q2", "2017 Q4")))
  expect_identical(+ yearquarter(x), yearquarter(x))
  expect_identical(1 + yearquarter(x), yearquarter(x) + 1)
  expect_error(yearquarter(x) - yearquarter(x), class = "vctrs_error_incompatible_op")
  expect_error(yearquarter(x) + yearquarter(x), class = "vctrs_error_incompatible_op")
})

test_that("vec_compare() for yearquarter()", {
  expect_identical(yearquarter(x) == yearquarter("2019 Q3"), c(TRUE, FALSE))
  expect_identical(yearquarter(x) <= yearquarter("2019 Q3"), c(TRUE, TRUE))
  expect_identical(yearquarter(x) > yearquarter("2019 Q3"), c(FALSE, FALSE))
  expect_identical(sort(yearquarter(x)), yearquarter(x[2:1]))
})

test_that("vec_cast() for yearquarter()", {
  expect_identical(as.Date(yearquarter(x)), dates)
  expect_identical(vec_cast(yearquarter(x), to = double()), as.double(yearquarter(x)))
  expect_identical(vec_cast(yearquarter(x), to = new_date()), dates)
  # expect_identical(as.POSIXct(yearquarter(x)), dttm)
  expect_identical(as.POSIXlt(yearquarter(x)), as.POSIXlt(dttm))
  # expect_identical(vec_cast(yearquarter(x), to = new_datetime()), dttm)
})

test_that("vec_c() for yearquarter()", {
  expect_identical(vec_c(dates, yearquarter(x)), rep(dates, times = 2))
  expect_identical(vec_c(yearquarter(x), dates), rep(dates, times = 2))
  # expect_identical(vec_c(dttm, yearquarter(x)), rep(dttm, times = 2))
  # expect_identical(vec_c(yearquarter(x), dttm), rep(dttm, times = 2))
  expect_identical(vec_c(dates, yearquarter(x)), c(dates, yearquarter(x)))
})
