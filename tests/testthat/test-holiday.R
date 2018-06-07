context("Australian national and state based public holiday")

nat_2018 <- tibble::tibble(
  holiday = c("New Year's Day", "Australia Day", "Good Friday", "Easter Monday", 
    "ANZAC Day", "Christmas Day", "Boxing Day"),
  date = as.Date(c("2018-01-01", "2018-01-26", "2018-03-30", "2018-04-02",
    "2018-04-25", "2018-12-25", "2018-12-26"))
)

test_that("National holidays", {
  expect_error(holiday_aus("2018"), "must be double/integer.")
  expect_equal(holiday_aus(2018), nat_2018)
})

easter_2018 <- tibble::tibble(
  holiday = c("Easter Saturday", "Easter Sunday"),
  date = as.Date(c("2018-03-31", "2018-04-01"))
)

queens_2018 <- tibble::tibble(
  holiday = "Queen's Birthday", date = as.Date("2018-06-11")
)

act_2018 <- tibble::tibble(
  holiday = c("Canberra Day", "Reconciliation Day"),
  date = as.Date(c("2018-03-12", "2018-05-28"))
)

nt_2018 <- tibble::tibble(
  holiday = c("May Day", "Picnic Day"),
  date = as.Date(c("2018-05-07", "2018-08-06"))
)

qld_2018 <- tibble::tibble(
  holiday = c("Labour Day", "Queen's Birthday"),
  date = as.Date(c("2018-05-07", "2018-10-01"))
)

sa_2018 <- tibble::tibble(
  holiday = c("Adelaide Cup Day", "Labour Day", "Proclamation Day"),
  date = as.Date(c("2018-03-12", "2018-10-01", "2018-12-26"))
)

tas_2018 <- tibble::tibble(
  holiday = "Eight Hours Day",
  date = as.Date("2018-03-12")
)

vic_2018 <- tibble::tibble(
  holiday = c("Labour Day", "Melbourne Cup"),
  date = as.Date(c("2018-03-12", "2018-11-06"))
)

wa_2018 <- tibble::tibble(
  holiday = c("Labour Day", "Western Australia Day"),
  date = as.Date(c("2018-03-05", "2018-06-04"))
)

nsw_2018 <- tibble::tibble(
  holiday = "Labour Day",
  date = as.Date("2018-10-01")
)

test_that("State-based holidays", {
  expect_equal(
    holiday_aus(2018, "ACT") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018) %>% 
      anti_join(queens_2018),
    act_2018
  )
  expect_equal(
    holiday_aus(2018, "NT") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018) %>% 
      anti_join(queens_2018),
    nt_2018
  )
  expect_equal(
    holiday_aus(2018, "QLD") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018),
    qld_2018
  )
  expect_equal(
    holiday_aus(2018, "SA") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018) %>% 
      anti_join(queens_2018),
    sa_2018
  )
  expect_equal(
    holiday_aus(2018, "TAS") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018) %>% 
      anti_join(queens_2018),
    tas_2018
  )
  expect_equal(
    holiday_aus(2018, "VIC") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018) %>% 
      anti_join(queens_2018),
    vic_2018
  )
  expect_equal(
    holiday_aus(2018, "WA") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018),
    wa_2018
  )
  expect_equal(
    holiday_aus(2018, "NSW") %>% 
      anti_join(nat_2018) %>% 
      anti_join(easter_2018) %>% 
      anti_join(queens_2018),
    nsw_2018
  )
})
