# sx_fct <- pedestrian %>%
#   mutate(Sensor = as.factor(Sensor)) %>%
#   filter(Sensor == "Southern Cross Station")
#
# test_that("group_split()", {
#   expect_identical(group_split(sx_fct)[[1]], sx_fct)
#   sx_lst <- group_split(sx_fct %>% group_by_key(), keep = FALSE)
#   expect_length(sx_lst, n_keys(sx_fct))
#   expect_equal(NROW(sx_lst[[1L]]), NROW(sx_fct))
#   expect_is(sx_lst[[1L]], "tbl_ts")
#   expect_equal(NCOL(sx_lst[[1L]]), NCOL(pedestrian) - 1L)
#   sx_lst2 <- group_split(sx_fct %>% group_by_key())
#   expect_equal(NCOL(sx_lst2[[1L]]), NCOL(pedestrian))
# })
#
# test_that("group_trim()", {
#   expect_equal(n_groups(group_trim(sx_fct %>% group_by_key())), 1L)
# })
