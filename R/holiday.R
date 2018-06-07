globalVariables("holiday")

#' Australian national and state-based public holiday
#'
#' @param year A vector of integer(s) indicating year(s).
#' @param state A state in Australia including "ACT", "NSW", "NT", "QLD", "SA", 
#' "TAS", "VIC", "WA", as well as "national".
#'
#' @return A tibble consisting of `holiday` labels and their associated dates 
#' in the year(s).
#'
#' @details 
#' Not documented public holidays:
#' * AFL public holidays for Victoria
#' * Queen's Birthday for Western Australia
#' * Royal Queensland Show for Queensland, which is for Brisbane only
#' @references 
#' [Public holidays](https://www.australia.gov.au/about-australia/special-dates-and-events/public-holidays)
#' @export
#'
#' @examples
#' holiday_aus(2016, state = "VIC")
#' holiday_aus(2013:2016, state = "ACT")
holiday_aus <- function(year, state = "national") {
  if (!is.numeric(year)) {
    abort("`year` must be double/integer.")
  }
  state <- match.arg(state, 
    c("national", "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"))
  nat <- holiday_aus_national(year)
  if (state == "VIC") {
    nat <- dplyr::bind_rows(nat, holiday_aus_vic(year))
  } else if (state == "NSW") {
    nat <- dplyr::bind_rows(nat, holiday_aus_nsw(year))
  } else if (state == "ACT") {
    nat <- dplyr::bind_rows(nat, holiday_aus_act(year))
  } else if (state == "NT") {
    nat <- dplyr::bind_rows(nat, holiday_aus_nt(year))
  } else if (state == "QLD") {
    nat <- dplyr::bind_rows(nat, holiday_aus_qld(year))
  } else if (state == "SA") {
    nat <- dplyr::bind_rows(nat, holiday_aus_sa(year)) %>% 
      mutate(holiday = ifelse(
        holiday == "Boxing Day", "Proclamation Day", holiday
      ))
  } else if (state == "TAS") {
    nat <- dplyr::bind_rows(nat, holiday_aus_tas(year))
  } else if (state == "WA") {
    nat <- dplyr::bind_rows(nat, holiday_aus_wa(year))
  }
  arrange(nat, date)
}

#' @importFrom lubridate month<-
holiday_aus_act <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 3) # Mar
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  march_1mon <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)
  # Find the second Monday in March (Canberra day)
  canb <- march_1mon + lubridate::weeks(1) 

  recon <- lubridate::make_date(year, 5, 27)
  diff_mon <- 2 - lubridate::wday(recon)
  recon <- recon + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  out <- tibble::tibble(
    holiday = rep(c("Canberra Day", "Reconciliation Day"), each = year_length),
    date = c(canb, recon)
  ) 
  recon_rm <- out %>% 
    filter(holiday == "Reconciliation Day", lubridate::year(date) < 2018)
  out %>% 
    anti_join(recon_rm, by = c("holiday", "date")) %>% 
    dplyr::bind_rows(easter_break(year), queens_birthday(year))
}

holiday_aus_nt <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 5) # May
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in May
  may_day <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  picnic <- lubridate::make_date(year, 8)
  diff_mon <- 2 - lubridate::wday(picnic)
  picnic <- picnic + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  tibble::tibble(
    holiday = rep(c("May Day", "Picnic Day"), each = year_length),
    date = c(may_day, picnic)
  )  %>% 
  dplyr::bind_rows(easter_break(year), queens_birthday(year))
}

holiday_aus_qld <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 5) # May
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in May
  may_day <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  # Find the first Monday in October
  month(starting) <- 10
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  queens <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  tibble::tibble(
    holiday = rep(c("Labour Day", "Queen's Birthday"), each = year_length),
    date = c(may_day, queens)
  )  %>% 
  dplyr::bind_rows(easter_break(year))
}

holiday_aus_sa <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 3) # March
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  mar_day <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)
  mar_cup <- mar_day + lubridate::weeks(1)

  month(starting) <- 5 # May
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  may_day <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)
  may_cup <- may_day + lubridate::weeks(2)

  month(starting) <- 10
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  labour <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  out <- tibble::tibble(
    holiday = rep(
      c(rep("Adelaide Cup Day", 2), "Labour Day"), each = year_length
    ),
    date = c(mar_cup, may_cup, labour)
  )
  mar_cup_rm <- out %>% 
    filter(
      holiday == "Adelaide Cup Day",
      lubridate::month(date) == 3,
      lubridate::year(date) < 2006 | lubridate::year(date) > 2019
    )
  may_cup_rm <- out %>% 
    filter(
      holiday == "Adelaide Cup Day",
      lubridate::month(date) == 5,
      lubridate::year(date) > 2005, lubridate::year(date) < 2020
    )
  out %>% 
    anti_join(mar_cup_rm, by = c("holiday", "date")) %>% 
    anti_join(may_cup_rm, by = c("holiday", "date")) %>% 
    dplyr::bind_rows(easter_break(year), queens_birthday(year))
}

holiday_aus_nsw <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 10) # Oct
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in Oct
  labour <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  tibble::tibble(
    holiday = rep("Labour Day", each = year_length),
    date = labour,
  ) %>% 
  dplyr::bind_rows(easter_break(year), queens_birthday(year))
}

holiday_aus_tas <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 3) # Mar
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  march_1mon <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)
  # Find the second Monday in March (labour day)
  labour <- march_1mon + lubridate::weeks(1) # VIC and Tas

  # Holiday labels
  tibble::tibble(
    holiday = rep("Eight Hours Day", each = year_length),
    date = labour
  ) %>% 
  dplyr::bind_rows(easter_break(year), queens_birthday(year))
}

holiday_aus_vic <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 3) # Mar
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  march_1mon <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)
  # Find the second Monday in March (labour day)
  labour <- march_1mon + lubridate::weeks(1) # VIC and Tas

  # AFL grand final day has become Public holiday in VIC since 2015
  # if (any(year == 2015)) {
  #   grand_final <- as_date("2015-10-02")
  # }
  # if (any(year == 2016)) {
  #   grand_final <- as_date("2016-09-30")
  # }

  month(starting) <- 11 # switch to Nov
  starting_wday <- lubridate::wday(starting)
  diff_tue <- 3 - starting_wday
  # Find the first Tuesday in Nov (Melbourne cup)
  melb_cup <- starting + ifelse(diff_tue >= 0, diff_tue, diff_tue + 7)

  # Holiday labels
  hdays_labels <- c("Labour Day", "Melbourne Cup")
  tibble::tibble(
    holiday = rep(hdays_labels, each = year_length),
    date = c(labour, melb_cup),
  ) %>% 
  dplyr::bind_rows(easter_break(year), queens_birthday(year))
}

holiday_aus_wa <- function(year) {
  year_length <- length(year)
  starting <- lubridate::make_date(year, 3) # Mar
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in March
  labour <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  month(starting) <- 6 # switch to June
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in June
  western <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)

  # Holiday labels
  hdays_labels <- c("Labour Day", "Western Australia Day")
  tibble::tibble(
    holiday = rep(hdays_labels, each = year_length),
    date = c(labour, western),
  ) %>% 
  dplyr::bind_rows(easter_break(year))
}

holiday_aus_national <- function(year) {
  year_length <- length(year)
  public_holidays <- vector(mode = "list", length = 6)
  counter <- incr(init = 1, size = 1)

  starting <- new_year <- lubridate::make_date(year) # new year's day
  new_year_wday <- lubridate::wday(new_year)
  # new_year day observed
  new_year <- as_date(ifelse(
    new_year_wday == 1, # Sunday
    new_year + lubridate::days(1), # shift to next Monday
    new_year
  ))
  new_year <- as_date(ifelse(
    new_year_wday == 7, # Saturday
    new_year + lubridate::days(2), # shift to next Monday
    new_year
  ))
  public_holidays[[counter()]] <- new_year

  australia <- lubridate::make_date(year, 1, 26) # australia day
  australia_wday <- lubridate::wday(australia)
  # australia day observed
  australia <- as_date(ifelse(
    australia_wday == 1, # Sunday
    australia + lubridate::days(1), # shift to next Monday
    australia
  ))
  australia <- as_date(ifelse(
    australia_wday == 7, # Saturday
    australia + lubridate::days(2), # shift to next Monday
    australia))
  public_holidays[[counter()]] <- australia

  good_friday <- as_date(timeDate::GoodFriday(year))
  public_holidays[[counter()]] <- good_friday
  easter_mon <- as_date(timeDate::Easter(year)) + lubridate::days(1)
  public_holidays[[counter()]] <- easter_mon

  anzac <- lubridate::make_date(year, 4, 25) # regardless of weekday
  public_holidays[[counter()]] <- anzac

  christmas <- lubridate::make_date(year, 12, 25)
  christmas_wday <- lubridate::wday(christmas)
  # Substitute for Sat/Sun 25 Dec
  christmas <- as_date(ifelse(
    christmas_wday == 1, # Sunday
    christmas + lubridate::days(2), # shift to next Tuesday
    christmas
  ))
  christmas <- as_date(ifelse(
    christmas_wday == 7, # Saturday
    christmas + lubridate::days(2), # shift to next Monday
    christmas
  ))
  public_holidays[[counter()]] <- christmas

  boxing <- lubridate::make_date(year, 12, 26)
  boxing_wday <- lubridate::wday(boxing)
  # Substitute for Sat/Sun 26 Dec
  boxing <- as_date(ifelse(
    boxing_wday == 1, # Sunday
    boxing + lubridate::days(1), # shift to next Monday
    boxing
  ))
  boxing <- as_date(ifelse(
    boxing_wday == 7, # Saturday
    boxing + lubridate::days(2), # shift to next Monday
    boxing
  ))
  public_holidays[[counter()]] <- boxing
  public_holidays <- as_date(unlist(public_holidays, recursive = FALSE))

  # Holiday labels
  hdays_labels <- c(
    "New Year's Day", "Australia Day",
    "Good Friday", "Easter Monday",
    "ANZAC Day", "Christmas Day", "Boxing Day"
  )
  tibble::tibble(
    holiday = rep(hdays_labels, each = year_length),
    date = public_holidays
  )
}

# Easter break: excluding Good Friday & Easter Monday
easter_break <- function(year) {
  easter_sun <- as_date(timeDate::Easter(year))
  easter_sat <- easter_sun - lubridate::days(1)

  # Holiday labels
  hdays_labels <- c("Easter Saturday", "Easter Sunday")
  tibble::tibble(
    holiday = rep(hdays_labels, each = length(year)),
    date = c(easter_sat, easter_sun),
  )
}

queens_birthday <- function(year) {
  starting <- lubridate::make_date(year, 6)
  starting_wday <- lubridate::wday(starting)
  diff_mon <- 2 - starting_wday
  # Find the first Monday in June
  june_1mon <- starting + ifelse(diff_mon >= 0, diff_mon, diff_mon + 7)
  # Find the second Monday in June (Queen's birthday)
  queens <- june_1mon + lubridate::weeks(1) # except QLD, WA
  tibble::tibble(
    holiday = rep("Queen's Birthday", each = length(year)),
    date = queens
  )
}
