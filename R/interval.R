# Number of time units
gen_interval <- function(x, exclude_zero = TRUE) {
  UseMethod("gen_interval")
}

gen_interval.numeric <- function(x, exclude_zero = TRUE) {
  min_interval(x, exclude_zero = exclude_zero) # num of years
}

gen_interval.POSIXt <- function(x, exclude_zero = TRUE) {
  dttm <- as.numeric(x)
  min_interval(dttm, exclude_zero = exclude_zero) # num of seconds
}

gen_interval.Date <- gen_interval.POSIXt

gen_interval.yearmth <- function(x, exclude_zero = TRUE) {
  # num of months
  mon <- lubridate::year(x) + (lubridate::month(x) - 1) / 12
  ceiling(min_interval(mon, exclude_zero = exclude_zero) * 12)
}

gen_interval.yearqtr <- function(x, exclude_zero = TRUE) {
  # num of quarters
  qtr <- lubridate::year(x) + (lubridate::quarter(x) - 1) / 4
  ceiling(min_interval(qtr, exclude_zero = exclude_zero) * 4)
}

# Assume date is regularly spaced
pull_interval <- function(x, exclude_zero = TRUE) {
  UseMethod("pull_interval")
}

pull_interval.POSIXt <- function(x, exclude_zero = TRUE) {
  nhms <- gen_interval.POSIXt(x, exclude_zero = exclude_zero)
  period <- split_period(nhms)
  structure(
    list(hour = period$hour, minute = period$minute, second = period$second),
    class = c("hms", "interval")
  )
}

pull_interval.Date <- function(x, exclude_zero = TRUE) {
  ndays <- gen_interval.Date(x, exclude_zero = exclude_zero)
  structure(list(day = ndays), class = c("day", "interval"))
}

pull_interval.yearmth <- function(x, exclude_zero = TRUE) {
  nmonths <- gen_interval.yearmth(x, exclude_zero = exclude_zero)
  structure(list(month = nmonths), class = c("month", "interval"))
}

pull_interval.yearqtr <- function(x, exclude_zero = TRUE) {
  nqtrs <- gen_interval.yearqtr(x, exclude_zero = exclude_zero)
  structure(list(quarter = nqtrs), class = c("quarter", "interval"))
}

pull_interval.numeric <- function(x, exclude_zero = TRUE) {
  nyrs <- gen_interval.numeric(x, exclude_zero = exclude_zero)
  structure(list(year = nyrs), class = c("year", "interval"))
}

# from ts time to dates
time_to_date <- function(x, ...) {
  UseMethod("time_to_date")
}

time_to_date.ts <- function(x, tz = "UTC", ...) {
  freq <- stats::frequency(x)
  time_x <- as.numeric(stats::time(x))
  if (freq == 12) { # monthly
    return(yearmth(time_x))
  } else if (freq == 4) { # quarterly
    return(yearqtr(time_x))
  } else if (freq == 1) { # yearly
    return(as.numeric(time_x))
  } else {
    return(lubridate::date_decimal(as.numeric(time_x), tz = tz))
  }
}

time_to_date.gts <- function(x, tz = "UTC", ...) {
  time_to_date(x$bts, tz = tz, ...)
}
