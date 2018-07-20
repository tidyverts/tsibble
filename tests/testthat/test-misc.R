context("pillar methods")
library(pillar)

a <- yearweek(seq(ymd("2017-02-01"), length.out = 12, by = "1 week"))
x <- yearmonth(seq(2010, 2012, by = 1 / 12))
y <- yearquarter(seq(2010, 2012, by = 1 / 4))

test_that("pillar S3 methods", {
  expect_equal(type_sum(a), "week")
  expect_equal(is_vector_s3(x), TRUE)
  expect_equal(type_sum(x), "mth")
  expect_equal(is_vector_s3(x), TRUE)
  expect_equal(type_sum(y), "qtr")
  expect_equal(is_vector_s3(y), TRUE)
  expect_equal(obj_sum(a), rep("week", length(a)))
  expect_equal(obj_sum(x), rep("mth", length(x)))
  expect_equal(obj_sum(y), rep("qtr", length(y)))

  expect_equal(type_sum(pedestrian), "tsibble")
})

tbl1 <- tsibble(
  date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
  value = rnorm(10),
  key = id(), index = date
)
tbl2 <- tsibble(
  qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30),
  key = id(group), index = qtr
)

test_that("tbl_sum.tbl_ts()", {
  expect_identical(tbl_sum(tbl1), c("A tsibble" = "10 x 2 [1D]"))
  expect_identical(
    tbl_sum(tbl1 %>% index_by(yrmth = yearmonth(date))),
    c("A tsibble" = "10 x 3 [1D]", "Groups" = "@ yrmth [1]")
  )
  expect_identical(
    tbl_sum(tbl2),
    c("A tsibble" = "30 x 3 [1Q]", "Key" = "group [3]")
  )
  expect_identical(
    tbl_sum(tbl2 %>% index_by(year = year(qtr))),
    c(
      "A tsibble" = "30 x 4 [1Q]",
      "Key" = "group [3]",
      "Groups" = "@ year [3]"
    )
  )
})

test_that("tbl_sum.grouped_ts()", {
  expect_identical(
    tbl_sum(tbl2 %>% group_by(group)),
    c(
      "A tsibble" = "30 x 3 [1Q]",
      "Key" = "group [3]",
      "Groups" = "group [3]"
    )
  )
  expect_identical(
    tbl_sum(tbl2 %>% index_by(year = year(qtr)) %>% group_by(group)),
    c(
      "A tsibble" = "30 x 4 [1Q]",
      "Key" = "group [3]",
      "Groups" = "group @ year [9]"
    )
  )
})


