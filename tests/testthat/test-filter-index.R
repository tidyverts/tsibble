context("filter_index()")

x <- c("2016", "2016-10", "2016-12-09")

test_that("class: Date", {
  expect_error(start(pedestrian$Date, 2017), "Must be")
  expect_equal(start(pedestrian$Date), as.Date("2015-01-01"))
  expect_equal(
    start(pedestrian$Date, x),
    as.Date(c("2016-01-01", "2016-10-01", "2016-12-09"))
  )
  expect_equal(end(pedestrian$Date), as.Date("2017-01-01"))
  expect_equal(
    end(pedestrian$Date, x),
    as.Date(c("2017-01-01", "2016-11-01", "2016-12-10"))
  )
})

y <- c("2016", "2016-10", "2016-12-09", "2016-12-09 10")
tz <- "Australia/Melbourne"

test_that("class: POSIXct", {
  expect_error(start(pedestrian$Date_Time, 2017), "Must be")
  expect_equal(start(pedestrian$Date_Time), as.POSIXct("2015-01-01", tz = tz))
  expect_equal(
    start(pedestrian$Date_Time, y),
    as.POSIXct(
      c("2016-01-01 00:00", "2016-10-01 00:00", "2016-12-09 00:00", "2016-12-09 10:00"),
      tz = tz
    )
  )
  expect_equal(end(pedestrian$Date_Time), as.POSIXct("2017-01-01", tz = tz))
  expect_equal(
    end(pedestrian$Date_Time, y),
    as.POSIXct(
      c("2017-01-01 00:00", "2016-11-01 00:00", "2016-12-09 01:00", "2016-12-09 11:00"),
      tz = tz
    )
  )
})

test_that("filter_index()", {
  expect_identical(
    pedestrian %>% 
      filter_index(~ "2015-02", "2015-08" ~ "2015-09", "2015-12" ~ .),
    pedestrian %>% 
      filter(
        Date_Time >= lubridate::ymd_h("2015-08-01 00", tz =tz) &
        Date_Time < lubridate::ymd_h("2015-10-01 00", tz = tz) |
        Date_Time <= lubridate::ymd_h("2015-02-28 23", tz = tz) | 
        Date_Time >= lubridate::ymd_h("2015-12-01 00", tz = tz)
      )
  )
  expect_identical(
    pedestrian %>% 
      filter_index("2015-02", "2015-08" ~ .),
    pedestrian %>% 
      filter_index("2015-02" ~ "2015-02", "2015-08" ~ .)
  )
  expect_identical(
    pedestrian %>% 
      filter_index(~ "2015-02"),
    pedestrian %>% 
      filter_index(. ~ "2015-02")
  )
  expect_identical(
    pedestrian %>% 
      filter_index(~ "2015"),
    pedestrian %>% 
      filter(lubridate::year(Date_Time) == 2015)
  )
  expect_identical(
    pedestrian %>% 
      filter_index("2015" ~ "2016"),
    pedestrian
  )
  expect_identical(
    pedestrian %>% 
      filter_index("2015-08" ~ "2015-02"),
    pedestrian[0L, ]
  )
})
