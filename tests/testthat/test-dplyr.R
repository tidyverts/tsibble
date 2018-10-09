library(lubridate)
library(dplyr)
context("dplyr verbs for tsibble")

pedestrian <- pedestrian %>%
  group_by(Sensor) %>%
  slice(1:10) %>%
  ungroup()

tourism <- tourism %>%
  group_by_key() %>%
  slice(1:10) %>%
  ungroup()

test_that("group_by()", {
  grped_df <- pedestrian %>%
    group_by(Date) %>%
    group_by(Sensor, add = TRUE)
  expect_length(group_vars(grped_df), 2)
  grped_df <- pedestrian %>%
    group_by(Sensor)
  expect_equal(n_groups(grped_df), 4)
  expect_length(group_size(grped_df), 4)
  expect_length(group_indices(grped_df), 40)
  expect_named(
    pedestrian %>% group_by(sensor = Sensor),
    c(names(pedestrian), "sensor")
  )
  expect_equal(group_vars(pedestrian %>% group_by(sensor = Sensor)), "sensor")
  expect_equal(
    group_vars(pedestrian %>% index_by(yearmonth(Date))),
    "yearmonth(Date)"
  )

  grped_t <- tourism %>%
    group_by(Purpose) %>%
    group_by(Region, State, add = TRUE)
  expect_length(group_vars(grped_t), 3)
  expect_equal(group_by_key(tourism), grped_t)
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
  expect_warning(tsbl1 <- arrange(tourism, Quarter), "Unexpected temporal order.")
  expect_equal(tsbl1, tourism)
  expect_false(is_ordered(tsbl1))
  expect_identical(key(tsbl1), key(tourism))
  expect_identical(groups(tsbl1), groups(tourism))
  tsbl2 <- arrange(tsbl1, Region, State, Purpose, Quarter)
  expect_identical(tsbl2, tourism)
})

idx_year <- seq.int(1970, 2010, by = 10)
dat_x <- tsibble(year = idx_year, value = rnorm(5), index = year)

test_that("warnings for arrange a univariate time series", {
  expect_warning(arrange(dat_x, value), "Unexpected temporal order.")
})

test_that("expect warnings from arrange.tbl_ts()", {
  expect_is(pedestrian %>% arrange(desc(Sensor), Date_Time), "tbl_ts")
  expect_warning(pedestrian %>% arrange(Time), "Unexpected temporal order.")
  expect_warning(pedestrian %>% arrange(Sensor, desc(Date_Time)), "Unexpected temporal order.")
  expect_warning(pedestrian %>% arrange(desc(Date_Time)), "Unexpected temporal order.")
  expect_warning(pedestrian %>% arrange(Count, Date_Time, Sensor), "Unexpected temporal order.")
  expect_warning(pedestrian %>% arrange(Sensor, Count, Date_Time), "Unexpected temporal order.")
  expect_warning(tbl <- pedestrian %>% arrange(Date_Time, Sensor), "Unexpected temporal order.")
  expect_identical(tbl %>% arrange(Sensor, Date_Time), pedestrian)
  bm <- pedestrian %>%
    filter(Sensor == "Birrarung Marr")
  expect_warning(bm %>% arrange(desc(Date_Time)), "Unexpected temporal order.")
})

test_that("arrange.grouped_ts()", {
  expect_warning(
    tsbl2 <- tourism %>% group_by(Region, State) %>% arrange(Quarter),
    "Unexpected temporal order."
  )
  expect_equal(tsbl2, tourism)
  expect_identical(key(tsbl2), key(tourism))
  expect_identical(group_vars(tsbl2), c("Region",  "State"))
  expect_warning(
    tsbl3 <- tourism %>%
      group_by(Region, State) %>%
      arrange(Quarter, .by_group = TRUE),
    "Unexpected temporal order."
  )
  expect_equal(tsbl3, tourism)
  expect_identical(key(tsbl3), key(tourism))
  expect_identical(group_vars(tsbl3), c("Region",  "State"))
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
  tsbl3 <- slice(tourism, 1:3)
  expect_identical(dim(tsbl3), c(3L, ncol(tourism)))
  tsbl4 <- tourism %>%
    group_by(Purpose) %>%
    slice(1:3)
  expect_identical(dim(tsbl4), c(12L, ncol(tourism)))
  expect_warning(slice(pedestrian, 3:1), "Unexpected temporal order.")
  expect_error(slice(pedestrian, c(3, 3)), "Duplicated")
  expect_error(slice(pedestrian, 3, 3), "only accepts one expression.")
})

test_that("select() and rename()", {
  expect_error(select(tourism, Quarter), "A valid tsibble")
  expect_error(select(tourism, Region), "A valid tsibble")
  expect_is(select(tourism, Region, .drop = TRUE), "tbl_df")
  expect_is(select(tourism, Region:Purpose), "tbl_ts")
  expect_is(select(tourism, Quarter:Purpose), "tbl_ts")
  expect_equal(
    quo_name(index(select(tourism, Index = Quarter, Region:Purpose))),
    "Index"
  )
  expect_equal(
    quo_name(index(select(tourism, Index = Quarter, Region:Purpose))),
    "Index"
  )
  expect_equal(
    format(key(select(tourism, Bottom = Region, Quarter, State:Purpose)))[[1]],
    "Bottom | State"
  )
  expect_equal(
    format(key(select(tourism, State, Region2 = Region, Purpose, Trips)))[[1]],
    "Region2 | State"
  )
  expect_equal(
    quo_name(index(rename(tourism, Index = Quarter))),
    "Index"
  )
  expect_equal(
    format(key(rename(tourism, Bottom = Region)))[[1]],
    "Bottom | State"
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
  expect_error(mutate(tourism, Quarter = 1), "A valid tsibble")
  expect_is(mutate(tourism, Quarter = 1, .drop = TRUE), "tbl_df")
  expect_error(mutate(tourism, Region = State), "A valid tsibble")
  expect_identical(ncol(mutate(tourism, New = 1)), ncol(tourism) + 1L)
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
  tsbl1 <- tourism %>%
    summarise(AllTrips = sum(Trips))
  expect_identical(ncol(tsbl1), 2L)
  tsbl2 <- tourism %>%
    group_by(Region, State, Purpose) %>%
    summarise(Trips = sum(Trips))
  expect_equal(tourism, tsbl2)
  expect_identical(key(tourism), key(tsbl2))
  expect_identical(index(tourism), index(tsbl2))
  expect_identical(is_regular(tourism), is_regular(tsbl2))
  tsbl3 <- tourism %>%
    group_by(Region, State, Purpose) %>%
    summarise(Obs = n())
  expect_identical(nrow(tsbl3), nrow(tourism))

  expect_error(pedestrian %>% summarise(month = yearmonth(Date_Time)))
  tbl_ped <- pedestrian %>%
    group_by(Date) %>%
    summarise(DailyCount = mean(Count), .drop = TRUE)
  expect_is(tbl_ped, "tbl_df")
})

tsbl <- tsibble(
  qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
  group = rep(c("x", "y", "z"), each = 10),
  a = rnorm(30),
  b = rnorm(30),
  c = rnorm(30),
  key = id(group), index = qtr
)

test_that("transmute()", {
  expect_equal(
    ncol(tourism %>% transmute(Region = paste(Region, State), .drop = TRUE)),
    1
  )
  out <- tourism %>% transmute(Region = paste(Region, State))
  expect_equal(ncol(out), 4)
  trans_tsbl <- tsbl %>% transmute(z = a / b)
  expect_equal(colnames(trans_tsbl), c("qtr", "group", "z"))
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
  is.Date(x) || is.character(x)
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

ref_tsbl <- tsbl %>%
  mutate_if(is.numeric, function(x) x + 1)

test_that("mutate() scoped variants", {
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

ref_tsbl <- tsbl %>%
  group_by(group) %>%
  summarise_if(is.numeric, sum)

test_that("summarise() scoped variants", {
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

dat_x <- tribble(
  ~ date, ~ bottom1, ~ group1, ~ bottom2, ~ group2, ~ value,
  ymd("2017-10-01"), 1, "a", "x", "z", 1,
  ymd("2017-10-02"), 1, "a", "x", "z", 1,
  ymd("2017-10-01"), 2, "a", "x", "z", 1,
  ymd("2017-10-02"), 2, "a", "x", "z", 1,
  ymd("2017-10-01"), 1, "a", "y", "z", 1,
  ymd("2017-10-02"), 1, "a", "y", "z", 1,
  ymd("2017-10-01"), 2, "a", "y", "z", 1,
  ymd("2017-10-02"), 2, "a", "y", "z", 1,
  ymd("2017-10-01"), 3, "b", "y", "z", 3,
  ymd("2017-10-02"), 3, "b", "y", "z", 3,
  ymd("2017-10-01"), 3, "b", "x", "z", 3,
  ymd("2017-10-02"), 3, "b", "x", "z", 3,
  ymd("2017-10-01"), 4, "b", "y", "z", 3,
  ymd("2017-10-02"), 4, "b", "y", "z", 3,
  ymd("2017-10-01"), 4, "b", "x", "z", 3,
  ymd("2017-10-02"), 4, "b", "x", "z", 3
)

tsbl <- as_tsibble(dat_x, key = id(bottom1 | group1, bottom2 | group2), index = date)

test_that("verbs for 2 nestings", {
  # key_remove
  sel_data <- select(tsbl, bottom1, bottom2)
  expect_named(sel_data, c("bottom1", "bottom2", "date"))
  expect_equal(unclass(key(sel_data)), rlang::syms(c("bottom1", "bottom2")))
  # key_rename
  sel_data2 <- select(tsbl, bot1 = bottom1, bot2 = bottom2)
  expect_named(sel_data2, c("bot1", "bot2", "date"))
  expect_equal(unclass(key(sel_data2)), rlang::syms(c("bot1", "bot2")))
})
