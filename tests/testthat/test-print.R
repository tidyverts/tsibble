tbl1 <- tsibble(
  date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
  value = rnorm(10),
  index = date
)
tbl2 <- tsibble(
  qtr = rep(yearquarter("2010 Q1") + 0:9, 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30),
  key = group, index = qtr
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
