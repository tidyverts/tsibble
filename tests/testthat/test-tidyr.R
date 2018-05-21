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
  expect_equal(format(key(out)), list())
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
  expect_error(tsbl %>% spread(qtr, value = value), "`key` must not be `qtr`,")
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
  expect_equal(key_size(out), rep(10, 3))
  out2 <- tsbl2 %>% 
    gather(key = key, value = value)
  expect_identical(out, out2)
})

test_that("nest()", {
 expect_error(pedestrian %>% nest(-Date_Time), "must nest the `index`") 
 expect_error(pedestrian %>% nest(Sensor), "must nest the `index`") 
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

test_that("unnest()", {
  expect_error(nest_t %>% unnest(Region), "Must contain a list-column")
  expect_error(
    nest_t %>% mutate(data2 = data) %>% unnest(),
    "Only accept one list-column of `tbl_ts`"
  )
  expect_error(nest_t %>% unnest(key = Region), "Have you forgotten")
  expect_error(nest_t %>% unnest(), "Invalid tsibble:")
  expect_is(nest_t %>% unnest(key = id(Region | State)), "tbl_ts")
  expect_equal(nest_t %>% unnest(key = id(Region | State)), tourism)
})
