x <- tsibble(
  year = rep(2016:2017, 2), grp = rep(letters[1:2], each = 2),
  key = grp, index = year
)

y_key <- tibble(grp = "a", upper = "A")
y_idx <- tibble(year = 2018, x = "fun")
z <- tibble(grp = c("a", "b"), upper = c("A", "B"))

test_that("left_join()", {
  left <- x %>% left_join(y_key)
  expect_is(left, "tbl_ts")
  expect_equal(dim(left), c(4, 3))
  expect_identical(key(x), key(left))
  expect_identical(index(x), index(left))
})

test_that("right_join()", {
  right <- x %>% right_join(y_key)
  expect_is(right, "tbl_ts")
  expect_equal(dim(right), c(2, 3))
  expect_identical(key(x), key(right))
  expect_identical(index(x), index(right))

  grped_right <- x %>%
    group_by(grp) %>%
    right_join(y_key)
  expect_is(grped_right, "grouped_ts")
  expect_equal(n_groups(grped_right), 1)
  expect_equal(group_size(grped_right), 2)
})

test_that("full_join()", {
  full <- x %>% full_join(y_idx)
  expect_is(full, "tbl_ts")
  expect_equal(dim(full), c(5, 3))
  expect_identical(key(x), key(full))
  expect_identical(index(x), index(full))
})

test_that("inner_join()", {
  inner <- x %>% inner_join(y_key)
  expect_is(inner, "tbl_ts")
  expect_equal(dim(inner), c(2, 3))
  expect_identical(key(x), key(inner))
  expect_identical(index(x), index(inner))
})

test_that("semi_join()", {
  semi <- x %>% semi_join(y_key)
  expect_is(semi, "tbl_ts")
  expect_equal(dim(semi), c(2, 2))
  expect_identical(key(x), key(semi))
  expect_identical(index(x), index(semi))
})

test_that("semi_join() #122", {
  semi <- pedestrian %>%
    semi_join(
      tibble(Sensor = "Birrarung Marr", Date_Time = Sys.time()),
      by = "Sensor"
    )
  expect_identical(semi, pedestrian %>% filter(Sensor == "Birrarung Marr"))
})

test_that("anti_join()", {
  anti <- x %>% anti_join(y_key)
  expect_is(anti, "tbl_ts")
  expect_equal(dim(anti), c(2, 2))
  expect_identical(key(x), key(anti))
  expect_identical(index(x), index(anti))
  expect_identical(x %>% anti_join(z), x[0, ])
})

test_that("mutual key and index #102", {
  xx <- x[1:2, ]
  yy <- x[c(1, 3), ]
  expect_identical(left_join(x, xx), x)
  expect_named(left_join(x, xx, by = "year"), c("year", "grp.x", "grp.y"))
  expect_named(left_join(x, yy, by = "grp"), c("year.x", "grp", "year.y"))
  expect_error(left_join(x, yy, by = "year"), "valid")
})

test_that("mutating joins abort for duplicates", {
  x <- tsibble(id = c("x", "y"), time = rep(Sys.Date(), 2), key = id)
  y <- tibble(id = rep("x", 2), time = rep(Sys.Date(), 2))
  expect_error(left_join(x, y), "valid")
  expect_error(right_join(x, y), "valid")
  expect_error(inner_join(x, y), "valid")
  expect_error(full_join(x, y), "valid")
})
