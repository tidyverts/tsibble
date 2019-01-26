context("tidyr verbs for tsibble")

tsbl <- tsibble(
  qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30),
  key = id(group), index = qtr
)

test_that("spread()", {
  out <- tsbl %>%
    spread(key = group, value = value)
  expect_is(out, "tbl_ts")
  expect_equal(key(out), list())
  expect_named(out, c("qtr", "x", "y", "z"))
  out_grp <- tsbl %>%
    group_by(group) %>%
    spread(key = group, value = value)
  expect_equal(groups(out_grp), NULL)
  out2 <- tourism %>%
    spread(key = Purpose, value = Trips)
  expect_equal(key_vars(out2), c("Region", "State"))
  expect_equal(ncol(out2), 7)
  out3 <- tourism %>%
    spread(key = State, value = Trips)
  expect_equal(key_vars(out3), c("Region", "Purpose"))
  expect_equal(ncol(out3), 10)
  expect_error(tsbl %>% spread(qtr, value = value), "can't be spread.")
  out4 <- tourism %>%
    group_by(Purpose) %>%
    spread(key = State, value = Trips)
  expect_is(out4, "grouped_ts")
  expect_equal(group_vars(out4), "Purpose")
  out5 <- tourism %>%
    index_by(year = year(Quarter)) %>%
    spread(key = State, value = Trips)
  expect_is(out5, "grouped_ts")
  expect_equal(group_vars(out5), "year")
})

tsbl2 <- tsbl %>%
  spread(key = group, value = value)

test_that("gather()", {
  out <- tsbl2 %>%
    gather(key = key, value = value, x:z)
  expect_equal(dim(out), c(30, 3))
  expect_equal(key_vars(out), "key")
  # expect_equal(key_size(out), rep(10, 3))
  out2 <- tsbl2 %>%
    gather(key = key, value = value)
  expect_identical(out, out2)
})

pedestrian <- pedestrian %>%
  group_by(Sensor) %>%
  slice(1:10) %>%
  ungroup()

tourism <- tourism %>%
  group_by_key() %>%
  slice(1:10) %>%
  ungroup()

test_that("nest()", {
 expect_error(pedestrian %>% nest(-Date_Time), "must be nested")
 expect_error(pedestrian %>% nest(Sensor), "must be nested")
 expect_named(pedestrian %>% nest(), "data")
 expect_named(pedestrian %>% nest(-Sensor), c("Sensor", "data"))
 expect_named(
   pedestrian %>% group_by(Sensor) %>% nest(),
   names(pedestrian %>% nest(-Sensor))
  )
 expect_named(pedestrian %>% nest(-Sensor, .key = "ts"), c("Sensor", "ts"))
 nested_ped <- pedestrian %>%
   nest(-Sensor)
 expect_is(nested_ped, "lst_ts")
 expect_equal(key_vars(nested_ped$data[[1]]), character(0))
})

nest_t <- tourism %>%
  nest(-Region, -State)

test_that("unnest.lst_ts()", {
  expect_error(nest_t %>% unnest(key = Region), "Key can only be created")
  expect_error(nest_t %>% unnest(), "is not a valid tsibble.")
  expect_is(nest_t %>% unnest(key = id(Region, State)), "tbl_ts")
  expect_equal(nest_t %>% unnest(key = id(Region, State)), tourism)
  expect_is(
    nest_t %>%
      mutate(data2 = lapply(data, as_tibble)) %>%
      unnest(key = id(Region, State)),
    "tbl_ts"
  )
})

nest2_t <- tourism %>% 
  group_by_key() %>% 
  summarise(
    value = list(quantile(Trips, c(0.3, 0.5, 0.7))),
    qtl = list(c(3, 5, 7))
  )

test_that("unnest.tbl_ts()", {
  expect_error(nest2_t %>% unnest(key = qtl), "Key can only be created")
  expect_error(nest2_t %>% unnest(), "is not a valid tsibble.")
  expect_is(nest2_t %>% unnest(key = id(qtl)), "tbl_ts")
  expect_equal(nest2_t %>% unnest(key = id(qtl)) %>% NCOL, 6)
})

test_that("dplyr verbs for lst_ts", {
  expect_error(
    nest_t %>% mutate(data2 = data) %>% unnest(),
    "accepts a list-column of `tbl_ts` to be unnested."
  )
  expect_named(
    nest_t %>% mutate(data2 = data) %>% unnest(data2, key = id(Region, State)),
    c("Region", "State", "Quarter", "Purpose", "Trips")
  )
  expect_is(unnest(nest_t %>% mutate(data = 1)), "tbl_df")
  expect_is(nest_t %>% select(data2 = data), "lst_ts")
  expect_is(nest_t %>% group_by(State), "grouped_df")
  expect_equal(
    nest_t %>% group_by(State) %>% mutate(Value = n()) %>% dplyr::pull(Value),
    as_tibble(nest_t) %>% group_by(State) %>% mutate(Value = n()) %>% dplyr::pull(Value)
  )
})

harvest <- tsibble(
  year = c(2011, 2013, 2014, 2010, 2012, 2014),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = id(fruit), index = year
)

harvest_fill <- fill_gaps(harvest, .full = TRUE)

test_that("fill()", {
  expect_equal(
    harvest_fill %>%
      group_by_key() %>%
      fill(kilo, .direction = "down"),
    harvest_fill %>% 
      as_tibble() %>% 
      group_by(fruit) %>% 
      fill(kilo, .direction = "down")
  )
})
