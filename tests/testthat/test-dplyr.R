library(lubridate)
context("dplyr verbs for tsibble")

test_that("group_by()", {
  expect_error(group_by(tourism, State | Region))
  expect_error(group_by(tourism, State | Region, Purpose))
  expect_error(group_by(tourism, Quarter))
  expect_error(group_by(pedestrian, Date_Time, Sensor))
  grped_df <- pedestrian %>% 
    group_by(Date) %>% 
    group_by(Sensor, add = TRUE)
  expect_length(group_vars(grped_df), 2)
  grped_df <- pedestrian %>% 
    group_by(Sensor)
  expect_equal(n_groups(grped_df), 4)
  expect_length(group_size(grped_df), 4)

  grped_t <- tourism %>% 
    group_by(Purpose) %>% 
    group_by(Region | State, add = TRUE)
  expect_length(group_vars(grped_t), 2)
})

test_that("arrange()", {
 tsbl1 <- arrange(tourism, Quarter)
 expect_equal(tsbl1, tourism)
 expect_identical(key(tsbl1), key(tourism))
 expect_identical(groups(tsbl1), groups(tourism))
 tsbl2 <- tourism %>%
   group_by(Region | State) %>%
   arrange(Quarter)
 expect_equal(tsbl2, tourism)
 expect_identical(key(tsbl2), key(tourism))
 expect_identical(unname(group_vars(tsbl2)), "Region | State")
})

test_that("filter() and slice()", {
  tsbl1 <- filter(tourism, Region == "Sydney", Purpose == "Holiday")
  expect_identical(key(tsbl1), key(tourism))
  expect_identical(groups(tsbl1), groups(tourism))
  expect_identical(index(tsbl1), index(tourism))
  expect_identical(tsbl1$Quarter, unique(tourism$Quarter))
  tsbl2 <- tourism %>%
    group_by(!!! key(.)) %>%
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
})

test_that("select() and rename()", {
  expect_error(select(tourism, Quarter))
  expect_error(select(tourism, Region))
  expect_is(select(tourism, Region, drop = TRUE), "tbl_df")
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
    key_vars(select(tourism, Bottom = Region, Quarter, State:Purpose))[[1]],
    "Bottom | State"
  )
  expect_equal(
    quo_name(index(rename(tourism, Index = Quarter))),
    "Index"
  )
  expect_equal(
    key_vars(rename(tourism, Bottom = Region))[[1]],
    "Bottom | State"
  )
})

test_that("mutate()", {
  expect_error(mutate(tourism, Quarter = 1))
  expect_is(mutate(tourism, Quarter = 1, drop = TRUE), "tbl_df")
  expect_error(mutate(tourism, Region = State))
  expect_identical(ncol(mutate(tourism, New = 1)), ncol(tourism) + 1L)
  tsbl <- tourism %>%
    group_by(!!! key(.)) %>%
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
    group_by(!!! key(.)) %>%
    summarise(Trips = sum(Trips))
  expect_equal(tourism, tsbl2)
  expect_identical(key(tourism), key(tsbl2))
  expect_identical(index(tourism), index(tsbl2))
  expect_identical(is_regular(tourism), is_regular(tsbl2))
  tsbl3 <- tourism %>%
    group_by(!!! key(.)) %>%
    summarise(Obs = n())
  expect_identical(nrow(tsbl3), nrow(tourism))

  expect_error(pedestrian %>% summarise(month = yearmonth(Date_Time)))
  tbl_ped <- pedestrian %>% 
    group_by(Date) %>% 
    summarise(DailyCount = mean(Count), drop = TRUE)
  expect_is(tbl_ped, "tbl_df")
})

test_that("transmute() and distinct()", {
  expect_error(tourism %>% transmute(Region = paste(Region, State)))
  expect_error(tourism %>% distinct(Region, State, Purpose))
})
