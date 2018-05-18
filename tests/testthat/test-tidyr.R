context("tidyr verbs for tsibble")

tsbl <- tsibble(
  qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30),
  key = id(group), index = qtr
)

test_that("spread()", {
  tsbl %>% 
    spread(key = group, value = value)
  grped_df <- pedestrian %>%
    group_by(Date) %>%
    group_by(Sensor, add = TRUE)
  expect_length(group_vars(grped_df), 2)
  grped_df <- pedestrian %>%
    group_by(Sensor)
  expect_equal(n_groups(grped_df), 4)
  expect_length(group_size(grped_df), 4)
  expect_length(group_indices(grped_df), 4)
  expect_equal(
    vapply(group_indices(grped_df), length, integer(1)),
    key_size(grped_df)
  )
  expect_named(
    pedestrian %>% group_by(sensor = Sensor),
    c(names(pedestrian), "sensor")
  )
  expect_equal(group_vars(pedestrian %>% group_by(sensor = Sensor)), "sensor")
  expect_equal(
    group_vars(pedestrian %>% group_by(yearmonth(Date))),
    "yearmonth(Date)"
  )

  grped_t <- tourism %>%
    group_by(Purpose) %>%
    group_by(Region, State, add = TRUE)
  expect_length(group_vars(grped_t), 3)
})
