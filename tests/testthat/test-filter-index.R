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

test_that("class: year*", {
  yrwk <- new_yearweek(unique(pedestrian$Date))
  expect_error(start(yrwk, 2017), "how to handle the numeric class")
  expect_equal(start(yrwk), yearweek(as.Date("2015-01-01")))
  expect_equal(
    start(yrwk, x),
    yearweek(as.Date(c("2015-12-28", "2016-09-26", "2016-12-05")))
  )
  expect_equal(end(pedestrian$Date), as.Date("2017-01-01"))
  expect_equal(
    end(pedestrian$Date, x),
    as.Date(c("2017-01-01", "2016-11-01", "2016-12-10"))
  )
  x <- yearquarter(c("2013 Q3", "2013 Qtr 3", "Quarter 4 2015"))
  expect_equal(time_in(x, ~ "2014 Q1"), c(TRUE, TRUE, FALSE))
  expect_equal(time_in(x, "2014 Q1" ~ .), c(FALSE, FALSE, TRUE))
})

si <- sessionInfo()
is_fedora <- grepl("Fedora", si$running, ignore.case = TRUE)
is_london <- Sys.timezone() == "Europe/London"

y <- c("2016", "2016-10", "2016-12-09", "2016-12-09 10")
tz <- "Australia/Melbourne"

test_that("class: POSIXct", {
  skip_if(is_london)
  expect_error(start(pedestrian$Date_Time, 2017), "Must be")
  expect_equal(start(pedestrian$Date_Time), ymd("2015-01-01", tz = tz))
  expect_equal(
    start(pedestrian$Date_Time, y),
    ymd_hm(
      c("2016-01-01 00:00", "2016-10-01 00:00", "2016-12-09 00:00", "2016-12-09 10:00"),
      tz = tz
    )
  )
  expect_equal(end(pedestrian$Date_Time), ymd_hms("2016-12-31 23:00:01", tz = tz))
  expect_equal(
    end(pedestrian$Date_Time, y),
    ymd_hms(
      c("2017-01-01 00:00:00", "2016-11-01 00:00:00", "2016-12-10 00:00:00", "2016-12-09 10:00:01"),
      tz = tz
    )
  )
})

test_that("class: hms", {
  z <- hms::as_hms(unique(pedestrian$Date_Time))
  expect_error(start(z, y = 11), "Must be")
  expect_equal(start(z), hms::as_hms("00:00:00"))
  # expect_error(start(z, y = "11:00"), "cannot be expressed as difftime")
  expect_equal(start(z, y = "11:00:00"), hms::as_hms("11:00:00"))
  expect_equal(end(z), hms::as_hms("23:00:01"))
  expect_equal(end(z, y = "21:00:00"), hms::as_hms("21:00:01"))
})

test_that("filter_index()", {
  skip_if(is_london)
  expect_identical(
    pedestrian %>%
      filter_index(~"2015-02", "2015-08" ~ "2015-09", "2015-12" ~ .),
    pedestrian %>%
      filter(
        Date_Time >= ymd_h("2015-08-01 00", tz = tz) &
          Date_Time < ymd_h("2015-10-01 00", tz = tz) |
          Date_Time <= ymd_h("2015-02-28 23", tz = tz) |
          Date_Time >= ymd_h("2015-12-01 00", tz = tz)
      )
  )
  expect_identical(
    pedestrian %>%
      filter_index(~"2015-02", "2015-08" ~ "2015-09", "2015-12" ~ .),
    pedestrian %>%
      filter_index(~"2015-Feb", "2015-Aug" ~ "2015-Sep", "2015-Dec" ~ .)
  )
  expect_identical(
    pedestrian %>%
      filter_index("2015-03-23 10" ~ "2015-10-31 12"),
    pedestrian %>%
      filter(
        Date_Time >= ymd_h("2015-03-23 10", tz = tz),
        Date_Time <= ymd_h("2015-10-31 12", tz = tz)
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
      filter_index(~"2015-02"),
    pedestrian %>%
      filter_index(. ~ "2015-02")
  )
  expect_identical(
    pedestrian %>%
      filter_index(~"2015"),
    pedestrian %>%
      filter(year(Date_Time) == 2015)
  )
  expect_equal(
    pedestrian %>%
      filter_index("2015" ~ "2016"),
    pedestrian
  )
  expect_identical(
    pedestrian %>%
      filter_index("2015-08" ~ "2015-02"),
    pedestrian[0L, ]
  )
  expect_equal(filter_index(pedestrian), pedestrian)
  ped_yr <- pedestrian %>%
    group_by(Sensor) %>%
    index_by(year = as.integer(year(Date_Time))) %>%
    summarise(cc = sum(Count))
  expect_identical(
    ped_yr %>% filter_index(2015),
    ped_yr %>% filter(year == 2015)
  )
})
