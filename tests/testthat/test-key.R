context("key for tsibble")

test_that("unkey()", {
  expect_error(unkey(pedestrian))
  sx <- pedestrian %>% filter(Sensor == "Southern Cross Station")
  expect_equal(key_vars(unkey(sx)), "NULL")
  unkey_sx <- unkey(sx) %>% unkey()
  expect_equal(key_vars(unkey_sx), "NULL")
})
