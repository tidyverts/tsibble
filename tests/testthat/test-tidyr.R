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
  out2 <- tourism %>% 
    spread(key = Purpose, value = Trips)
  expect_equal(key_vars(out2), c("Region", "State"))
  expect_equal(ncol(out2), 7)
  out3 <- tourism %>% 
    spread(key = State, value = Trips)
  expect_equal(key_vars(out3), c("Region", "Purpose"))
  expect_equal(ncol(out3), 10)
  expect_error(tsbl %>% spread(qtr, value = value), "`key` must not be `qtr`,")
})

tsbl2 <- tsbl %>% 
  spread(key = group, value = value)

test_that("gather()", {
  out <- tsbl2 %>% 
    gather(key = key, value = value, x:z)
  expect_equal(dim(out), c(30, 3))
  expect_equal(key_vars(out), "key")
  expect_equal(key_size(out), rep(10, 3))
})
