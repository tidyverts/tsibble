context("rolling tsibble")

harvest <- tsibble(
  year = rep(2010:2012, 2),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = id(fruit), index = year
)

test_that("error for existing `.id`", {
  expect_error(
    slide_tsibble(harvest, .size = 2, .id = "kilo"),
    "Can't overwrite existing column `kilo`."
  )
})

test_that("slide_tsibble()", {
  res <- slide_tsibble(harvest, .size = 2)
  expect_equal(NROW(res), NROW(harvest) + 2L)
  expect_named(res, c(names(harvest), ".id"))
  expect_equal(res$.id, c(rep(1:2, each = 4)))
})

test_that("tile_tsibble()", {
  res <- tile_tsibble(harvest, .size = 2, .id = "tile_id")
  expect_equal(NROW(res), NROW(harvest))
  expect_named(res, c(names(harvest), "tile_id"))
  expect_equal(res$tile_id, c(rep(1, 4), rep(2, 2)))
})

test_that("stretch_tsibble()", {
  res <- stretch_tsibble(harvest)
  expect_equal(NROW(res), NROW(harvest) * 2)
  expect_equal(res$.id, c(rep(1, 2), rep(2, 4), rep(3, 6)))
})
