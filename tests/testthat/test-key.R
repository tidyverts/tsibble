context("key for tsibble")

test_that("unkey()", {
  expect_error(unkey(pedestrian))
  sx <- pedestrian %>% filter(Sensor == "Southern Cross Station")
  expect_equal(key_vars(unkey(sx)), character(0))
  unkey_sx <- unkey(sx) %>% unkey()
  expect_equal(key_vars(unkey_sx), character(0))
})

test_that("tsibble_rename()", {
  bm <- pedestrian %>%
    filter(Sensor == "Birrarung Marr") %>%
    unkey()
  key_bm <- tsibble_rename(bm, "sensor" = "Sensor")
  expect_equal(key_vars(key_bm), character(0))
  expect_true("sensor" %in% names(key_bm))
  key_t <- tourism %>%
    tsibble_rename("purpose" = "Purpose", "region" = "Region", "trip" = "Trips")
})

test_that("key_reduce()", {
  melb <- tourism %>%
    filter(Region == "Melbourne")
  expect_error(melb %>% select(-Purpose), "A valid tsibble")
})
