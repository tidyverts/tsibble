context("ggplot()")

library(ggplot2)
test_that("not break ggplot()", {
  expect_is(ggplot(pedestrian, aes(x = Date_Time, y = Count)), "ggplot")
})
