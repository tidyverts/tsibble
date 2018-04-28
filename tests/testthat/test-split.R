context("split_by()")

test_that("Empty quosure", {
 x_lst <- pedestrian %>% split_by()
 expect_is(x_lst, "list")
 expect_length(x_lst, 1)
 expect_equal(dim(x_lst[[1]]), dim(pedestrian))
})

test_that("Split by bare variables", {
  sensor_lst <- pedestrian %>% split_by(Sensor)
  expect_is(sensor_lst, "list")
  expect_length(sensor_lst, n_keys(pedestrian))
  lst_size <- vapply(sensor_lst, nrow, integer(1))
  expect_equal(lst_size, key_size(pedestrian))
  expect_is(sensor_lst[[1]], "tbl_ts")
  expect_identical(
    sensor_lst[[1]],
    filter(pedestrian, Sensor == "Birrarung Marr")
  )
})

test_that("Split by nesting variables", {
  expect_error(tourism %>% split_by(Region | State), "not found")
})
