x <- c("2019 Sep", "2018 Jan")
dates <- as.Date(c("2019-09-01", "2018-01-01"))
dttm <- as.POSIXct(c("2019-09-01", "2018-01-01"), tz = "UTC")

test_that("input types for yearmonth()", {
  expected <- yearmonth(x)
  expect_identical(yearmonth(dttm), expected)
  expect_identical(yearmonth(dates), expected)
  expect_identical(yearmonth(expected), expected)
  expect_identical(yearmonth(x), expected)
  expect_identical(yearmonth(), expected[0])
  expect_identical(yearmonth(c(596, 576)), expected)
})

test_that("vec_arith() for yearmonth()", {
  expect_identical(yearmonth(x) + 1:2, yearmonth(c("2019 Oct", "2018 Mar")))
  expect_identical(yearmonth(x) - 1, yearmonth(c("2019 Aug", "2017 Dec")))
  expect_identical(+ yearmonth(x), yearmonth(x))
  expect_identical(- yearmonth(x), yearmonth(x))
  expect_identical(1 + yearmonth(x), yearmonth(x) + 1)
  expect_equal(yearmonth(x) - yearmonth(x), rep(0, 2))
  expect_error(yearmonth(x) + yearmonth(x), class = "vctrs_error_incompatible_op")
})

test_that("vec_compare() for yearmonth()", {
  expect_identical(yearmonth(x) == yearmonth("2019 Sep"), c(TRUE, FALSE))
  expect_identical(yearmonth(x) <= yearmonth("2019 Sep"), c(TRUE, TRUE))
  expect_identical(yearmonth(x) > yearmonth("2019 Sep"), c(FALSE, FALSE))
  expect_identical(sort(yearmonth(x)), yearmonth(x[2:1]))
})

test_that("vec_cast() for yearmonth()", {
  expect_identical(as.Date(yearmonth(x)), dates)
  expect_identical(as.character(yearmonth(x)), x)
  expect_identical(vec_cast(yearmonth(x), to = double()), as.double(yearmonth(x)))
  expect_identical(vec_cast(yearmonth(x), to = new_date()), dates)
  expect_identical(vec_data(as.POSIXct(yearmonth(x))), vec_data(dttm)) # tzone
  expect_identical(as.POSIXlt(yearmonth(x)), as.POSIXlt(dttm))
  expect_identical(
    vec_data(vec_cast(yearmonth(x), to = new_datetime())),
    vec_data(dttm))
})

test_that("vec_c() for yearmonth()", {
  expect_identical(vec_c(dates, yearmonth(x)), rep(dates, times = 2))
  expect_identical(vec_c(yearmonth(x), dates), rep(dates, times = 2))
  expect_identical(
    vec_data(vec_c(dttm, yearmonth(x))),
    vec_data(rep(dttm, times = 2)))
  expect_identical(
    vec_data(vec_c(yearmonth(x), dttm)),
    vec_data(rep(dttm, times = 2)))
  expect_identical(
    vec_data(vec_c(dates, yearmonth(x))),
    vec_data(c(dates, yearmonth(x))))
})

test_that("yearmonth() #89", {
  expect_false(
    anyNA(yearmonth.yearmon(as.numeric(time(
      ts(rnorm(139), frequency = 12, start = c(1978, 2))
    ))))
  )
})

test_that("yearmonth() #226", {
  expect_equal(yearmonth("2020-01-01"), yearmonth("2020-01-02"))
})
