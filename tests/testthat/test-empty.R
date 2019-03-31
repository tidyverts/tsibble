context("handle empty tsibble and empty groups")

test_that("create an empty tsibble", {
  expect_identical(interval(tsibble(datetime = Sys.time()[0])), init_interval())
  expect_identical(
    interval(tsibble(datetime = Sys.time()[0], regular = FALSE)),
    irregular()
  )
})

ped_null <- pedestrian %>%
  filter(Sensor == 0)

test_that("dplyr verbs for empty tsibble", {
  expect_equal(NROW(ped_null), 0L)
  expect_is(ped_null %>% group_by(Sensor), "grouped_ts")
  expect_equal(NROW(ped_null %>% mutate(Count1 = Count + 1)), 0L)
  expect_equal(NROW(ped_null %>% transmute(Count1 = Count + 1)), 0L)
  expect_equal(NROW(ped_null %>% summarise(Count1 = sum(Count))), 0L)
  expect_warning(ped_null %>% arrange(Count), "Unexpected temporal ordering.")
  # expect_identical(ped_null %>% arrange(Count), ped_null)
  expect_identical(ped_null %>% slice(0), ped_null)
  expect_identical(ped_null %>% filter(Sensor == 0), ped_null)
  expect_identical(ped_null[0, ], ped_null)
})

test_that("tsibble verbs empty tsibble", {
  expect_identical(fill_gaps(ped_null), ped_null)
  expect_identical(has_gaps(ped_null), tibble(.gaps = FALSE))
  expect_equal(NROW(count_gaps(ped_null)), 0)
})

ped_tsbl <- pedestrian %>%
  mutate(Sensor = as.factor(Sensor)) %>%
  filter_index("2015-01")

ped_grped <- pedestrian %>%
  mutate(Sensor = as.factor(Sensor)) %>%
  group_by_key() %>%
  filter_index("2015-01") %>%
  as_tibble()

test_that("dplyr verbs for empty groups (factors)", {
  expect_identical(
    key_data(ped_tsbl %>% mutate(x = 1L)),
    group_data(ped_grped %>% mutate(x = 1L))
  )
  k <- key_data(ped_tsbl %>% group_by_key() %>% slice(1:2))
  g <- group_data(ped_grped %>% slice(1:2))
  expect_identical(k$Sensor, g$Sensor)
  expect_identical(k$.rows, g$.rows)
  expect_identical(
    key_data(ped_tsbl %>% arrange(Sensor, Date_Time)),
    group_data(ped_grped %>% arrange(Sensor, Date_Time))
  )
})

ped_tsbl <- pedestrian %>%
  filter_index("2015-01", .preserve = TRUE)

ped_grped <- pedestrian %>%
  group_by_key() %>%
  filter_index("2015-01", .preserve = TRUE) %>%
  as_tibble()

test_that("dplyr verbs for empty groups (characters)", {
  expect_identical(
    key_data(ped_tsbl %>% mutate(x = 1L)),
    group_data(ped_grped %>% mutate(x = 1L))
  )
  k1 <- key_data(ped_tsbl %>% group_by_key() %>% slice(1:2))
  g1 <- group_data(ped_grped %>% slice(1:2))
  expect_identical(k1$Sensor, g1$Sensor)
  expect_identical(k1$.rows, g1$.rows)
  expect_identical(
    key_data(ped_tsbl %>% arrange(Sensor, Date_Time)),
    group_data(ped_grped %>% arrange(Sensor, Date_Time))
  )
})

x <- tibble(
  groups = rep(factor("a", levels = letters[1:2]), 10),
  index = as.Date("2019-01-01") + 0:9
)

test_that("dplyr verbs for empty groups (factors)", {
  y <- as_tsibble(x, key = groups, .drop = FALSE)
  expect_equal(n_keys(y), 2L)
  expect_false(key_drop_default(y))
  expect_equal(n_keys(y %>% mutate(a = 1)), 2L)
  expect_equal(n_keys(y %>% mutate(groups = groups)), 2L)
  expect_equal(n_keys(y %>% mutate(groups = 1L)), 1L)
  expect_equal(n_keys(y %>% select(index, groups)), 2L)
  expect_equal(n_keys(y %>% rename(index2 = index, groups2 = groups)), 2L)
})
