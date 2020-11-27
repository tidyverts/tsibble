library(lubridate)
library(dplyr)

pedestrian <- pedestrian %>%
  group_by(Sensor) %>%
  slice(1:48) %>%
  ungroup()

tourism <- tourism %>%
  group_by_key() %>%
  slice(1:10) %>%
  ungroup()

test_that("group_by()", {
  expect_error(group_by(pedestrian, Date_Time), "a grouping variable.")
  expect_error(group_by(pedestrian, Date_Time, Sensor), "a grouping variable.")
  grped_df <- pedestrian %>%
    group_by(Date) %>%
    group_by(Sensor, .add = TRUE)
  expect_length(group_vars(grped_df), 2)
  expect_identical(group_vars(grped_df), c("Date", "Sensor"))
  grped_df <- pedestrian %>%
    group_by(Sensor)
  expect_equal(n_groups(grped_df), 4)
  expect_length(group_size(grped_df), 4)
  expect_length(group_indices(grped_df), NROW(grped_df))
  expect_named(
    pedestrian %>% group_by(sensor = Sensor),
    c(names(pedestrian), "sensor")
  )
  expect_equal(group_vars(pedestrian %>% group_by(sensor = Sensor)), "sensor")
  expect_equal(
    group_vars(pedestrian %>% index_by(yearmonth(Date))),
    "yearmonth(Date)"
  )
})

test_that("group_by_key()", {
  grped_t <- tourism %>%
    group_by(Purpose) %>%
    group_by(Region, State, .add = TRUE)
  expect_length(group_vars(grped_t), 3)
  expect_true(all(is.element(group_vars(group_by_key(tourism)), group_vars(grped_t))))

  expect_identical(
    pedestrian %>% index_by(Date) %>% group_by_key() %>% index2_var(),
    "Date"
  )
})

test_that("ungroup()", {
  grped_df <- pedestrian %>%
    index_by(Date)
  expect_identical(ungroup(grped_df), pedestrian)
  grped_df2 <- pedestrian %>%
    index_by(Date) %>%
    group_by(Sensor)
  expect_identical(ungroup(grped_df), pedestrian)
})

test_that("arrange.tbl_ts()", {
  expect_equal(arrange(tourism), tourism)
  expect_equal(arrange(tourism %>% group_by(Purpose)), group_by(tourism, Purpose))
  tsbl1 <- arrange(tourism, Quarter)
  expect_true(is_ordered(tsbl1))
  expect_identical(key(tsbl1), key(tourism))
  expect_identical(groups(tsbl1), groups(tourism))
  tsbl2 <- arrange(tsbl1, Region, State, Purpose, Quarter)
  expect_identical(tsbl2, tourism)
})

idx_year <- seq.int(1970, 2010, by = 10)
dat_x <- tsibble(year = idx_year, value = 5:1, index = year)

warn_msg <- "Current temporal order."
test_that("warnings for arrange a univariate time series", {
  expect_warning(arrange(dat_x, value), warn_msg)
})

test_that("expect warnings from arrange.tbl_ts()", {
  expect_s3_class(pedestrian %>% arrange(desc(Sensor), Date_Time), "tbl_ts")
  expect_warning(pedestrian %>% arrange(Time), warn_msg)
  expect_warning(pedestrian %>% arrange(Sensor, desc(Date_Time)), warn_msg)
  expect_warning(pedestrian %>% arrange(desc(Date_Time)), warn_msg)
  expect_warning(pedestrian %>% arrange(Count, Date_Time, Sensor), warn_msg)
  expect_warning(pedestrian %>% arrange(Sensor, Count, Date_Time), warn_msg)
  tbl <- pedestrian %>% arrange(Date_Time, Sensor)
  expect_identical(tbl %>% arrange(Sensor, Date_Time), pedestrian)
  bm <- pedestrian %>% filter(Sensor == "Birrarung Marr")
  expect_warning(bm %>% arrange(desc(Date_Time)), warn_msg)
})

test_that("arrange.grouped_ts()", {
  tsbl2 <- tourism %>%
    group_by(Region, State) %>%
    arrange(Quarter)
  expect_identical(key(tsbl2), key(tourism))
  expect_identical(group_vars(tsbl2), c("Region", "State"))
  tsbl3 <- tourism %>%
    group_by(Region, State) %>%
    arrange(Quarter, .by_group = TRUE)
  expect_equal(tsbl3, tourism %>% arrange(Region, State, Quarter), ignore_attr = TRUE)
  expect_identical(key(tsbl3), key(tourism))
  expect_identical(group_vars(tsbl3), c("Region", "State"))
  tsbl4 <- tourism %>%
    group_by(Region, State) %>%
    arrange(Purpose, Quarter, .by_group = TRUE)
  expect_equal(tsbl4, group_by(tourism, Region, State))
})

test_that("filter() and slice()", {
  tsbl1 <- filter(tourism, Region == "Sydney", Purpose == "Holiday")
  expect_identical(key(tsbl1), key(tourism))
  expect_identical(groups(tsbl1), groups(tourism))
  expect_identical(index(tsbl1), index(tourism))
  expect_identical(tsbl1$Quarter, unique(tourism$Quarter))
  tsbl2 <- tourism %>%
    group_by(Region, State, Purpose) %>%
    filter(Quarter < yearquarter(ymd("1999-01-01")))
  expect_identical(key(tsbl2), key(tourism))
  expect_identical(group_vars(tsbl2), key_vars(tourism))
  expect_identical(index(tsbl2), index(tourism))
  expect_error(slice(tourism, rep(1, 2)), "Duplicated")
  tsbl3 <- slice(tourism, 1:3)
  expect_identical(dim(tsbl3), c(3L, ncol(tourism)))
  tsbl4 <- tourism %>%
    group_by(Purpose) %>%
    slice(1:3)
  expect_identical(dim(tsbl4), c(12L, ncol(tourism)))
  expect_identical(slice(pedestrian, NA), slice(pedestrian, 0L))
  expect_identical(slice(pedestrian, c(1, NA)), slice(pedestrian, 1L))
  expect_identical(slice(pedestrian, c(1, NA, 100000)), slice(pedestrian, 1L))
  expect_warning(slice(pedestrian, 3:1), warn_msg)
  expect_identical(slice(pedestrian, -(1:10)), pedestrian[-(1:10), ])
  expect_identical(slice(pedestrian, -(10:1)), slice(pedestrian, -(1:10)))
})

test_that("filter() and slice() with .preserve = TRUE", {
  ped_fil1 <- pedestrian %>% filter_index("2015-01", .preserve = TRUE)
  ped_fil2 <- pedestrian %>%
    group_by_key() %>%
    filter_index("2015-01", .preserve = TRUE) %>%
    as_tibble()
  expect_identical(key_data(ped_fil1)$.rows, group_data(ped_fil2)$.rows)
  expect_identical(key_rows(ped_fil1), group_rows(ped_fil2))
  res <- as_tsibble(AirPassengers) %>%
    filter_index(~ "1949 Mar", .preserve = TRUE)
  expect_false(identical(key_rows(res), key_rows(as_tsibble(AirPassengers))))
})

test_that("select() and rename()", {
  expect_s3_class(select(tourism, Region:Purpose), "tbl_ts")
  expect_s3_class(select(tourism, Quarter:Purpose), "tbl_ts")
  expect_equal(
    quo_name(index(select(tourism, Index = Quarter, Region:Purpose))),
    "Index"
  )
  expect_named(
    select(tourism, Index = Quarter, Trip = Trips, Region:Purpose),
    c("Index", "Trip", "Region", "State", "Purpose")
  )
  expect_identical(rename(tourism), tourism)
  tourism_mel <- tourism %>%
    filter(Region == "Melbourne", Purpose == "Holiday")
  cols <- names(tourism_mel)
  expect_named(select(tourism_mel, -Region), cols[-2])
  expect_named(select(tourism_mel, -State), cols[-3])
  expect_named(select(tourism_mel, -Purpose), cols[-4])
  expect_named(select(tourism_mel, Trips), c("Trips", "Quarter"))
  expect_named(select(tourism_mel, -Region, -State, -Purpose), cols[c(1, 5)])
})

test_that("rename tsibble with 'ordered' #126", {
  foo <- tsibble(date = as.Date("2017-01-01") + 0:2, x = 1:3)
  expect_identical(
    attr(attr(foo %>% rename(the_date = date), "index"), "ordered"),
    is_ordered(foo)
  )
})

test_that("select() with group_by()", {
  # grouped by "key"
  grped_ped <- pedestrian %>% group_by(Sensor)
  expect_equal(groups(grped_ped), groups(grped_ped %>% select(Sensor, Date_Time)))
  sel_ped <- grped_ped %>%
    select(Key = Sensor, Date_Time)
  expect_equal("Key", group_vars(sel_ped))
  rned_ped <- grped_ped %>%
    rename(Key = Sensor)
  expect_equal("Key", group_vars(rned_ped))

  grped_ped2 <- pedestrian %>% group_by(Date)
  sel2 <- grped_ped2 %>%
    select(Date2 = Date, Key = Sensor, Date_Time)
  expect_equal("Date2", group_vars(sel2))
  expect_equal("Key", key_vars(sel2))
  sel3 <- grped_ped2 %>%
    select(Date2 = Date, Sensor, Date_Time)
  expect_equal("Date2", group_vars(sel3))
  ren2 <- grped_ped2 %>% rename(Date2 = Date)
  expect_equal("Date2", group_vars(ren2))
})

test_that("mutate()", {
  expect_error(mutate(tourism, Quarter = NULL), "can't be removed.")
  expect_error(mutate(tourism, Quarter = 1), "is not a valid tsibble.")
  expect_error(mutate(tourism, Region = State), "is not a valid tsibble.")
  expect_identical(ncol(mutate(tourism, New = 1)), ncol(tourism) + 1L)
  expect_equal(ncol(mutate(tourism, State = NULL)), 4)
  expect_equal(
    interval(tourism %>% mutate(Quarter = as.Date(Quarter))),
    new_interval(day = 1)
  )
  expect_equal(
    interval(new_data(tourism) %>% mutate(Trips2 = 1)),
    new_interval(quarter = 1)
  )
  tsbl <- tourism %>%
    group_by(Region, State, Purpose) %>%
    mutate(New = 1:n())
  tsbl1 <- filter(tsbl, New == 1)
  tsbl2 <- slice(tsbl, 1)
  expect_equal(tsbl1, tsbl2)
  expect_identical(key(tsbl1), key(tsbl2))
  expect_identical(groups(tsbl1), groups(tsbl2))
  expect_identical(index(tsbl1), index(tsbl2))
})

test_that("summarise()", {
  expect_error(tourism %>% summarise(qtl = quantile(Trips)), "valid")
  tsbl1 <- tourism %>%
    summarise(AllTrips = sum(Trips))
  expect_identical(ncol(tsbl1), 2L)
  tsbl2 <- tourism %>%
    group_by(Region, State, Purpose) %>%
    summarise(Trips = sum(Trips))
  expect_equal(tourism %>% select(names(tsbl2)), tsbl2, ignore_attr = TRUE)
  expect_identical(key(tourism), key(tsbl2))
  expect_identical(index(tourism), index(tsbl2))
  expect_identical(is_regular(tourism), is_regular(tsbl2))
  tsbl3 <- tourism %>%
    group_by(Region, State, Purpose) %>%
    summarise(Obs = n())
  expect_identical(nrow(tsbl3), nrow(tourism))
})

tsbl <- tsibble(
  qtr = rep(yearquarter("2010 Q1") + 0:9, 3),
  group = rep(c("x", "y", "z"), each = 10),
  a = rnorm(30),
  b = rnorm(30),
  c = rnorm(30),
  key = group, index = qtr
)

test_that("transmute()", {
  out <- tourism %>% transmute(Region = paste(Region, State))
  expect_equal(ncol(out), 4)
  trans_tsbl <- tsbl %>% transmute(z = a / b)
  expect_equal(colnames(trans_tsbl), c("qtr", "group", "z"))
})

test_that("transmute.grouped_ts()", {
  out <- pedestrian %>%
    group_by(Time) %>%
    transmute()
  expect_equal(ncol(out), 3)
  out2 <- pedestrian %>%
    group_by_key() %>%
    transmute()
  expect_named(out2, c("Date_Time", "Sensor"))
})

test_that("distinct()", {
  expect_equal(
    tourism %>% distinct(Region, State, Purpose),
    as_tibble(tourism) %>% distinct(Region, State, Purpose)
  )
})

test_that("rename() scoped variants", {
  all_names <- toupper(names(tsbl))
  expect_equal(names(rename_all(tsbl, toupper)), all_names)
  at_names <- c("qtr", "group", "A", "b", "C")
  expect_equal(names(rename_at(tsbl, vars(a, c), toupper)), at_names)
  if_names <- c("qtr", "group", LETTERS[1:3])
  expect_equal(names(rename_if(tsbl, is.numeric, toupper)), if_names)
})

date_character <- function(x) {
  is_yearquarter(x) || is.character(x)
}

test_that("select() scoped variants", {
  all_names <- names(tsbl)
  expect_equal(names(select_all(tsbl)), all_names)
  expect_equal(names(select_all(tsbl, toupper)), toupper(all_names))
  at_names <- c("qtr", "group", "b")
  expect_equal(names(select_at(tsbl, at_names)), at_names)
  if_names <- c("qtr", "group")
  expect_equal(names(select_if(tsbl, date_character)), if_names)
})

test_that("mutate() scoped variants", {
  ref_tsbl <- tsbl %>%
    mutate_if(is.numeric, function(x) x + 1)
  expect_equal(
    tsbl %>%
      mutate_if(is.numeric, function(x) x + 1),
    ref_tsbl
  )
  expect_equal(
    tsbl %>%
      mutate_at(vars(a:c), function(x) x + 1),
    ref_tsbl
  )
})

test_that("summarise() scoped variants", {
  ref_tsbl <- tsbl %>%
    group_by(group) %>%
    summarise_if(is.numeric, sum)
  expect_equal(
    tsbl %>%
      group_by(group) %>%
      summarise_if(is.numeric, sum),
    ref_tsbl
  )
  expect_equal(
    tsbl %>%
      group_by(group) %>%
      summarise_at(vars(a:c), sum),
    ref_tsbl
  )
})

test_that("index_by() + summarise() for grouping factors #197", {
  expect_silent({
    tourism %>%
      mutate(across(where(is.character), as.factor)) %>%
      index_by(Year = year(Quarter)) %>%
      group_by(Region, State) %>%
      summarise(Trips = sum(Trips))
  })
})

test_that("rename() for renaming key", {
  bm <- pedestrian %>%
    filter(Sensor == "Birrarung Marr") %>%
    update_tsibble(key = NULL)
  key_bm <- rename(bm, "sensor" = "Sensor")
  expect_equal(key_vars(key_bm), character(0))
  expect_true("sensor" %in% names(key_bm))
  key_t <- tourism %>%
    rename("purpose" = "Purpose", "region" = "Region", "trip" = "Trips")
})

test_that("drop redundant key #196", {
  sim_tourism <- tourism %>%
    filter(Purpose == "Holiday") %>%
    select(-Purpose)
  expect_equal(key_vars(sim_tourism), c("Region", "State"))
})
