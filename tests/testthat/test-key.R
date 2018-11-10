context("key for tsibble")

test_that("key_by()", {
  expect_error(key_by(pedestrian), "Can't retain")
  sx <- pedestrian %>% filter(Sensor == "Southern Cross Station")
  expect_equal(key_vars(key_by(sx)), character(0))
  unkey_sx <- key_by(sx) %>% key_by()
  expect_equal(key_vars(unkey_sx), character(0))
})

test_that("rename_tsibble()", {
  bm <- pedestrian %>%
    filter(Sensor == "Birrarung Marr") %>%
    key_by()
  key_bm <- rename_tsibble(bm, "sensor" = "Sensor")
  expect_equal(key_vars(key_bm), character(0))
  expect_true("sensor" %in% names(key_bm))
  key_t <- tourism %>%
    rename_tsibble("purpose" = "Purpose", "region" = "Region", "trip" = "Trips")
})

test_that("key_reduce()", {
  melb <- tourism %>%
    filter(Region == "Melbourne")
  expect_error(melb %>% select(-Purpose), "Can't retain")
})
