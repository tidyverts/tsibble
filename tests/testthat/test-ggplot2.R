library(ggplot2)
test_that("not break ggplot()", {
  expect_s3_class(ggplot(pedestrian, aes(x = Date_Time, y = Count)), "ggplot")
})
