context("key for tsibble")

test_that("rename_tsibble()", {
  bm <- pedestrian %>%
    filter(Sensor == "Birrarung Marr") %>%
    update_tsibble(key = NULL)
  key_bm <- rename_tsibble(bm, "sensor" = "Sensor")
  expect_equal(key_vars(key_bm), character(0))
  expect_true("sensor" %in% names(key_bm))
  key_t <- tourism %>%
    rename_tsibble("purpose" = "Purpose", "region" = "Region", "trip" = "Trips")
})

test_that("key_reduce()", {
  melb <- tourism %>%
    filter(Region == "Melbourne")
  expect_error(melb %>% select(-Purpose), "is not a valid tsibble.")
})
