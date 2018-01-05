context("Test tsummarise()")

idx_second <- seq(
  ymd_hms("2017-01-01 00:00:01"),
  ymd_hms("2017-01-01 00:00:05"),
  by = 1
)
dat_x <- tibble(
  date_time = idx_second,
  value = 1
)
tsbl1 <- as_tsibble(dat_x, index = date_time)

test_that("From seconds to higher date", {
  res1 <- tsummarise(tsbl1,
    date_min = ceiling_date(date_time, unit = "min"),
    value = sum(value)
  )
  expect_equal(
    as_tibble(res1),
    tibble(date_min = ymd_hm("2017-01-01 00:01"), value = 5)
  )
  res2 <- tsummarise(tsbl1,
    date_min = ceiling_date(date_time, unit = "hour"),
    value = sum(value)
  )
  expect_equal(
    as_tibble(res2),
    tibble(date_min = ymd_h("2017-01-01 01"), value = 5)
  )
  res3 <- tsummarise(tsbl1,
    date_min = floor_date(date_time, unit = "day"),
    value = sum(value)
  )
  expect_equal(
    as_tibble(res3),
    tibble(date_min = ymd_h("2017-01-01 0"), value = 5)
  )
})

idx_day <- seq.Date(ymd("2017-01-01"), ymd("2017-01-20"), by = 4)
dat_x <- tibble(
  date = idx_day,
  value = 1
)
tsbl2 <- as_tsibble(dat_x, index = date)

test_that("From Date to year-month, year-quarter and year", {
  res1 <- tsummarise(tsbl2, yrmth = yearmonth(date), value = sum(value))
  expect_equal(
    as_tibble(res1),
    tibble(yrmth = yearmonth(ymd("2017-01-01")), value = 5)
  )
  res2 <- tsummarise(tsbl2, yrqtr = yearquarter(date), value = sum(value))
  expect_equal(
    as_tibble(res2),
    tibble(yrqtr = yearquarter(ymd("2017-01-01")), value = 5)
  )
  res3 <- tsummarise(tsbl2, yr = year(date), value = sum(value))
  expect_equal(
    as_tibble(res3),
    tibble(yr = year(ymd("2017-01-01")), value = 5)
  )
  res4 <- tsummarise(res1, yrqtr = yearquarter(yrmth), value = sum(value))
  expect_equal(res2, res4)
  res5 <- tsummarise(res2, yr = year(yrqtr), value = sum(value))
  expect_equal(res3, res5)
})

dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rep(1:2, each = 5)
)
tsbl3 <- as_tsibble(dat_x, key = id(group), index = date)

test_that("tsummarise for grouped_ts", {
  res1 <- tsbl3 %>%
    group_by(group) %>%
    tsummarise(value = sum(value), yrmth = yearmonth(date))
  expect_is(res1, "grouped_ts")
  expect_equal(
    as_tibble(res1),
    tibble(
      group = c("a", "b"),
      yrmth = yearmonth(ymd("2017-01-01")),
      value = c(5L, 10L)
    )
  )
})
