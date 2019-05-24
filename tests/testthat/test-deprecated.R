context("deprecated functions and arguments")

test_that("corrupt tsibble object", {
  attr(pedestrian, "regular") <- TRUE
  expect_warning(format(pedestrian), "corrupt tsibble")
  attr(tourism, "ordered") <- TRUE
  expect_warning(format(tourism), "corrupt tsibble")
})

test_that("as.tsibble()", {
  expect_warning(as.tsibble(AirPassengers), "as_tsibble()")
})

test_that("id()", {
  expect_warning(
    as_tsibble(AirPassengers) %>% update_tsibble(key = id()),
    "`key = NULL`"
  )
  expect_warning(
    pedestrian %>% update_tsibble(key = id(Sensor)),
    "`key = Sensor`"
  )
  expect_warning(
    tourism %>% update_tsibble(key = id(Region, Purpose)),
    " deprecated"
  )
})
