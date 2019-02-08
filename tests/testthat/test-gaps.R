context("fill_gaps() & count_gaps() for a tsibble")

idx_day <- seq.Date(ymd("2017-01-01"), ymd("2017-01-20"), by = 4)
dat_x <- tibble(
  date = idx_day,
  value = rnorm(5)
)

test_that("a tbl_df/data.frame", {
  expect_error(fill_gaps(dat_x), "data.frame")
})

test_that("unknown interval", {
  tsbl <- as_tsibble(dat_x[1, ], index = date)
  expect_identical(fill_gaps(tsbl), tsbl)
  expect_equal(count_gaps(tsbl)$.n, integer(0))
  expect_identical(scan_gaps(pedestrian[0L, ]), pedestrian[0L, 1:2])
  expect_equal(has_gaps(tsbl), tibble(".gaps" = FALSE))
})

test_that("an irregular tbl_ts", {
  tsbl <- as_tsibble(dat_x, index = date, regular = FALSE)
  expect_error(fill_gaps(tsbl), "irregular")
  expect_error(count_gaps(tsbl), "irregular")
  expect_error(has_gaps(tsbl), "irregular")
})

test_that("a tbl_ts without implicit missing values", {
  tsbl <- as_tsibble(dat_x, index = date)
  expect_identical(fill_gaps(tsbl), tsbl)
  ref_tbl <- tibble(
    .from = as.Date(character(0)), .to = as.Date(character(0)),
    .n = integer(0)
  )
  expect_identical(count_gaps(tsbl), ref_tbl)
})

daylight <- pedestrian %>%
  filter(
    Sensor == "Birrarung Marr",
    Date == lubridate::ymd("20151004", tz = "Australia/Melbourne")
  )
standard <- pedestrian %>%
  filter(
    Sensor == "Birrarung Marr",
    Date == lubridate::ymd("20150405", tz = "Australia/Melbourne")
  )

test_that("daylight saving", {
  expect_identical(NROW(fill_gaps(daylight)), 23L)
  expect_identical(NROW(fill_gaps(standard)), 25L)
})

dat_y <- dat_x[c(1:3, 5), ]
tsbl <- as_tsibble(dat_y, index = date)

test_that("a tbl_ts of 4 day interval with no replacement", {
  full_tsbl <- fill_gaps(tsbl)
  expect_identical(dim(full_tsbl), c(5L, 2L))
  expect_equal(
    as_tibble(full_tsbl[4, ]),
    tibble(date = ymd("2017-01-13"), value = NA_real_)
  )
})

test_that("a tbl_ts of 4 day interval with value replacement", {
  full_tsbl <- fill_gaps(tsbl, value = 0)
  expect_equal(
    as_tibble(full_tsbl[4, ]),
    tibble(date = ymd("2017-01-13"), value = 0)
  )
})

test_that("a tbl_ts of 4 day interval with bad names", {
  expect_error(fill_gaps(tsbl, value1 = value), "Unknown column")
})

test_that("a tbl_ts of 4 day interval with function replacement", {
  full_tsbl <- fill_gaps(tsbl, value = sum(value))
  expect_equal(
    as_tibble(full_tsbl[4, ]),
    tibble(date = ymd("2017-01-13"), value = sum(tsbl$value))
  )
})

dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rep(1:2, each = 5)
)
dat_y <- dat_x[c(2:8, 10), ]
tsbl <- as_tsibble(dat_y, key = id(group), index = date)

test_that("fill_gaps() for corner case", {
  expect_identical(fill_gaps(tsbl[1:5, ]), tsbl[1:5, ])
})

tourism <- tourism %>%
  group_by_key() %>%
  slice(1:10) %>%
  ungroup()

test_that("fill_gaps() for yearquarter", {
  full_tsbl <- tourism %>%
    fill_gaps()
  expect_is(full_tsbl$Quarter, "yearquarter")
})

test_that("fill_gaps() for a grouped_ts", {
  full_tsbl <- tsbl %>%
    group_by(group) %>%
    fill_gaps(value = sum(value), .full = TRUE)
  expect_warning(
    tourism %>% group_by(Quarter) %>% fill_gaps(Trips = sum(Trips)),
    "`.data` is a complete tsibble."
  )
  expect_identical(dim(full_tsbl), c(10L, 3L))
  expect_equal(
    as_tibble(full_tsbl[c(1, 9), ]),
    tibble(
      date = c(ymd("2017-01-01"), ymd("2017-01-13")),
      group = c("a", "b"),
      value = c(4L, 8L)
    )
  )
})

test_that("fill.tbl_ts(.full = TRUE)", {
  full_tsbl <- tsbl %>%
    fill_gaps(.full = TRUE) %>%
    group_by(group) %>%
    tidyr::fill(value, .direction = "up")
  expect_equal(
    as_tibble(full_tsbl[c(1, 9), ]),
    tibble(
      date = c(ymd("2017-01-01"), ymd("2017-01-13")),
      group = c("a", "b"),
      value = c(1L, 2L)
    )
  )
})

test_that("fill.tbl_ts(.full = FALSE)", {
  full_tsbl <- tsbl %>%
    fill_gaps() %>%
    group_by(group) %>%
    tidyr::fill(value, .direction = "up")
  expect_equal(
    as_tibble(full_tsbl[8, ]),
    tibble(
      date = ymd("2017-01-13"),
      group = "b",
      value = 2L
    )
  )
})

test_that("count_gaps.tbl_ts(.full = TRUE)", {
  full_tbl <- tsbl  %>% count_gaps(.full = TRUE)
  expect_equal(
    full_tbl,
    tibble(
      group = c("a", "b"),
      .from = c(ymd("2017-01-01"), ymd("2017-01-13")),
      .to = c(ymd("2017-01-01"), ymd("2017-01-13")),
      .n = c(1L, 1L)
    )
  )
})

# harvest <- tsibble(
#   year = c(2010, 2011, 2012, 2011, 2012, 2014),
#   fruit = rep(c("kiwi", "cherry"), each = 3),
#   kilo = sample(1:10, size = 6),
#   key = id(fruit), index = year
# )
#
# test_that("count_gaps.tbl_ts(.full = TRUE, .common = TRUE)", {
#   expect_equal(NROW(count_gaps(harvest, .common = TRUE)), 0)
#   expect_equal(NROW(count_gaps(harvest, .common = TRUE, .full = TRUE)), 2)
# })

test_that("count_gaps.tbl_ts(.full = FALSE)", {
  full_tbl <- tsbl %>% count_gaps()
  b <- tibble(
    group = "b",
    .from = ymd("2017-01-13"),
    .to = ymd("2017-01-13"),
    .n = 1L
  )
  expect_equal(full_tbl, b)
})

harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 2012, 2013),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = id(fruit), index = year
)

test_that("has_gaps()", {
  expect_equal(has_gaps(harvest)$.gaps, c(FALSE, TRUE))
  expect_equal(has_gaps(harvest, .full = TRUE)$.gaps, c(TRUE, TRUE))
})

test_that("Error in tbl_gaps()", {
  expect_error(tbl_gaps(x = 1:4, y = 1:3), "must not be greater than")
})

test_that("seq_generator()", {
  x <- nanotime::nanotime("1970-01-01T00:00:00.000000001+00:00") + c(0:3, 5:9)
  expect_length(seq_generator(x), 10)
  y <- structure(c("x", "y"), class = "xxx")
  pull_interval.xxx <- function(x) {init_interval(unit = 1)}
  expect_error(seq_generator(y, pull_interval(y)), "defined")
})
