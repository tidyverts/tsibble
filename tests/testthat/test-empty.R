context("handle empty tsibble")

test_that("create an empty tsibble", {
  expect_identical(interval(tsibble(datetime = Sys.time()[0])), init_interval())
  expect_identical(
    interval(tsibble(datetime = Sys.time()[0], regular = FALSE)),
    irregular()
  )
})

ped_null <- pedestrian %>% 
  filter(Sensor == 0)

test_that("dplyr verbs", {
  expect_equal(NROW(ped_null), 0L)
  expect_is(ped_null %>% group_by(Sensor), "grouped_ts")
  expect_equal(NROW(ped_null %>% mutate(Count1 = Count + 1)), 0L)
  expect_equal(NROW(ped_null %>% transmute(Count1 = Count + 1)), 0L)
  expect_equal(NROW(ped_null %>% summarise(Count1 = sum(Count))), 0L)
  expect_identical(ped_null %>% arrange(Count), ped_null)
  expect_identical(ped_null %>% slice(0), ped_null)
  expect_identical(ped_null %>% filter(Sensor == 0), ped_null)
  expect_identical(ped_null[0, ], ped_null)
})

test_that("tsibble verbs", {
  expect_identical(fill_gaps(ped_null), ped_null)
  expect_identical(has_gaps(ped_null), tibble(.gaps = FALSE))
  expect_equal(NROW(count_gaps(ped_null)), 0)
})
