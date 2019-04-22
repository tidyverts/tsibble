context("bind tsibble")

idx_day <- seq.Date(ymd("2017-02-01"), ymd("2017-02-05"), by = 1)
dat_x <- tibble(
  date = rep(idx_day, 2),
  group = rep(letters[1:2], each = 5),
  value = rnorm(10)
)

tsbl <- as_tsibble(dat_x, key = group, index = date)
tsbl2 <- mutate(tsbl, date = date + rep(5:9, each = 2), value2 = 2)

test_that("rbind()", {
  expect_error(rbind(tsbl, tsbl), "is not a valid tsibble.")
  expect_identical(rbind(tsbl[1, ], tsbl[-1, ]), tsbl)
  expect_is(rbind(tsbl, tsbl2), "tbl_ts")
})

vic <- tourism %>%
  filter(State == "Victoria")
nsw <- tourism %>%
  filter(State == "New South Wales")

test_that("rbind() for custom index class #78", {
  res <- rbind(vic, nsw)
  expect_is(res$Quarter, "yearquarter")
})

test_that("rbind() for mixed intervals", {
  res <- rbind(update_tsibble(vic, regular = FALSE), nsw)
  expect_identical(interval(res), interval(nsw))
  res2 <- rbind(
    update_tsibble(vic, regular = FALSE), 
    update_tsibble(nsw, regular = FALSE)
  )
  expect_identical(format(interval(res2)), "!")
})

test_that("cbind()", {
  expect_is(cbind(tsbl, tsbl), "tbl_ts")
})
