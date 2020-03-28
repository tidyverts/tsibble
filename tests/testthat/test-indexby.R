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

test_that("illegal input in index_by()", {
  expect_identical(group_vars(tsbl1 %>% index_by()), "date_time")
  expect_error(tsbl1 %>% index_by("date_time"), "Unsupported index type:")
  expect_error(tsbl1 %>% index_by(date_time = date_time), "be overwritten.")
  expect_error(
    tsbl1 %>% index_by(as.Date(date_time), yearmonth(date_time)),
    "only accepts one expression or empty."
  )
})

test_that("From seconds to higher date", {
  res1 <- tsbl1 %>%
    index_by(date_min = ceiling_date(date_time, unit = "min")) %>%
    summarise(value = sum(value))
  expect_equal(
    as_tibble(res1),
    tibble(date_min = ymd_hm("2017-01-01 00:01"), value = 5)
  )
  res2 <- tsbl1 %>%
    index_by(date_min = ceiling_date(date_time, unit = "hour")) %>%
    summarise(value = sum(value))
  expect_equal(
    as_tibble(res2),
    tibble(date_min = ymd_h("2017-01-01 01"), value = 5)
  )
  res3 <- tsbl1 %>%
    index_by(date_min = floor_date(date_time, unit = "day")) %>%
    summarise(value = sum(value))
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

test_that("From Date to year-week, year-month, year-quarter and year", {
  res0 <- tsbl2 %>%
    index_by(yrwk = yearweek(date)) %>%
    summarise(value = sum(value))
  expect_equal(
    as_tibble(res0),
    tibble(yrwk = yearweek(ymd(idx_day[-4])), value = c(1, 1, 2, 1))
  )
  res1 <- tsbl2 %>%
    index_by(yrmth = yearmonth(date)) %>%
    summarise(value = sum(value))
  expect_equal(
    as_tibble(res1),
    tibble(yrmth = yearmonth(ymd("2017-01-01")), value = 5)
  )
  res2 <- tsbl2 %>%
    index_by(yrqtr = yearquarter(date)) %>%
    summarise(value = sum(value))
  expect_equal(
    as_tibble(res2),
    tibble(yrqtr = yearquarter(ymd("2017-01-01")), value = 5)
  )
  res3 <- tsbl2 %>%
    index_by(yr = year(date)) %>%
    summarise(value = sum(value))
  expect_equal(
    as_tibble(res3),
    tibble(yr = year(ymd("2017-01-01")), value = 5)
  )
  res4 <- res1 %>%
    index_by(yrqtr = yearquarter(yrmth)) %>%
    summarise(value = sum(value))
  expect_equal(res2, res4)
  res5 <- res2 %>%
    index_by(yr = year(yrqtr)) %>%
    summarise(value = sum(value))
  expect_equal(res3, res5)
})

dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rep(1:2, each = 5)
)
tsbl3 <- as_tsibble(dat_x, key = group, index = date)

test_that("index_by() with group_by()", {
  res1 <- tsbl3 %>%
    group_by(group) %>%
    index_by(yrmth = yearmonth(date)) %>%
    summarise(value = sum(value))
  expect_is(res1, "tbl_ts")
  expect_equivalent(
    as_tibble(res1),
    tibble(
      group = c("a", "b"),
      yrmth = yearmonth(ymd("2017-01-01")),
      value = c(5L, 10L)
    )
  )
})

tsbl4 <- tsibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value1 = rep(1:2, each = 5),
  value2 = rnorm(10),
  value3 = rnorm(10),
  key = group, index = date
)

test_that("summarise scoped variants", {
  ts_all <- tsbl4 %>%
    index_by(date2 = yearmonth(date)) %>%
    summarise_all(.funs = "mean")
  expect_named(ts_all, c("date2", "value1", "value2", "value3"))
  expect_equal(nrow(ts_all), 1)
  ts_all <- tsbl4 %>%
    index_by(yrmth = yearmonth(date)) %>%
    summarise_all(.funs = "mean")
  expect_is(ts_all[["yrmth"]], "yearmonth")
  expect_named(ts_all, c("yrmth", "value1", "value2", "value3"))
  ts_if <- tsbl4 %>%
    index_by(date2 = yearmonth(date)) %>%
    summarise_if(.predicate = is.numeric, .funs = mean)
  expect_is(ts_if[["date2"]], "yearmonth")
  expect_named(ts_if, c("date2", "value1", "value2", "value3"))
  expect_equal(nrow(ts_if), 1)
  ts_at <- tsbl4 %>%
    index_by(date2 = yearmonth(date)) %>%
    summarise_at(.vars = c("value1", "value3"), .funs = mean)
  expect_is(ts_at[["date2"]], "yearmonth")
  expect_named(ts_at, c("date2", "value1", "value3"))
  expect_equal(nrow(ts_at), 1)
})

test_that("scoped variants with group_by()", {
  ts_all <- tsbl4 %>%
    group_by(group) %>%
    index_by(yrmth = yearmonth(date)) %>%
    summarise_all(.funs = "mean")
  expect_named(ts_all, c("group", "yrmth", "value1", "value2", "value3"))
  expect_equal(nrow(ts_all), 2)
  ts_if <- tsbl4 %>%
    group_by(group) %>%
    index_by(yrmth = yearmonth(date)) %>%
    summarise_if(.predicate = is.numeric, .funs = mean)
  expect_named(ts_if, c("group", "yrmth", "value1", "value2", "value3"))
  expect_equal(nrow(ts_if), 2)
  ts_at <- tsbl4 %>%
    group_by(group) %>%
    index_by(yrmth = yearmonth(date)) %>%
    summarise_at(.vars = c("value1", "value3"), .funs = mean)
  expect_named(ts_at, c("group", "yrmth", "value1", "value3"))
  expect_equal(nrow(ts_at), 2)
  tbl <- tourism %>%
    group_by(Region, State) %>%
    index_by(Year = year(Quarter)) %>%
    summarise_if(.predicate = is.numeric, .funs = mean)
  expect_named(tbl, c("Region", "State", "Year", "Trips"))
})

test_that("index_by() with pedestrian", {
  ped_idx <- pedestrian %>%
    index_by(yrmth = yearmonth(Date))
  expect_is(ped_idx, "grouped_ts")
  expect_identical(index2(ped_idx), rlang::sym("yrmth"))
  expect_named(ped_idx, c(names(pedestrian), "yrmth"))
  ped_fil <- ped_idx %>%
    filter(Date_Time == min(Date_Time))
  ped_ref <- as_tibble(pedestrian) %>%
    group_by(yrmth = yearmonth(Date)) %>%
    filter(Date_Time == min(Date_Time))
  expect_equivalent(ped_fil, ped_ref)
  ped_ren <- ped_fil %>%
    rename(yrmth2 = yrmth)
  expect_identical(index2(ped_ren), rlang::sym("yrmth2"))
  ped_sum <- ped_ren %>%
    summarise(Total = sum(Count))
  expect_named(ped_sum, c("yrmth2", "Total"))
  expect_identical(index(ped_sum), rlang::sym("yrmth2"))
  expect_identical(index2(ped_sum), index(ped_sum))
  ped_sum2 <- ped_ren %>%
    group_by(Sensor) %>%
    summarise(Total = sum(Count))
  expect_named(ped_sum2, c("Sensor", "yrmth2", "Total"))
  expect_identical(index(ped_sum2), rlang::sym("yrmth2"))
  expect_identical(index2(ped_sum2), index(ped_sum2))
  expect_identical(groups(ped_sum2), NULL)
  ped_mut <- pedestrian %>%
    index_by(Date) %>%
    mutate(ttl = sum(Count), prop = Count / ttl)
  expect_identical(group_vars(ped_mut), "Date")
  ped_sum3 <- ped_mut %>%
    summarise(ttl_prop = sum(prop))
  expect_equal(format(interval(ped_sum3)), "1D")
})

test_that("index_by() with lambda expression", {
  expect_identical(
    pedestrian %>% index_by(yrmth = ~ yearmonth(.)),
    pedestrian %>% index_by(yrmth = yearmonth(Date_Time))
  )
})
