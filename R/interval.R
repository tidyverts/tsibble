# Number of time units
find_interval <- function(x, exclude_zero = TRUE) {
  UseMethod("find__interval")
}

find_interval.numeric <- function(x, exclude_zero = TRUE) {
  min_interval(x, exclude_zero = exclude_zero)
}

find_interval.integer <- find_interval.numeric

find_interval.year <- find_interval.numeric

find_interval.POSIXt <- function(x, exclude_zero = TRUE) {
  dttm <- as.numeric(x)
  min_interval(dttm, exclude_zero = exclude_zero) # num of seconds
}

find_interval.Date <- find_interval.POSIXt

find_interval.yearmth <- function(x, exclude_zero = TRUE) {
  # num of months
  mon <- lubridate::year(x) + (lubridate::month(x) - 1) / 12
  ceiling(min_interval(mon, exclude_zero = exclude_zero) * 12)
}

find_interval.yearqtr <- function(x, exclude_zero = TRUE) {
  # num of quarters
  qtr <- lubridate::year(x) + (lubridate::quarter(x) - 1) / 4
  ceiling(min_interval(qtr, exclude_zero = exclude_zero) * 4)
}

# Assume date is regularly spaced
pull_interval <- function(x, exclude_zero = TRUE) {
  UseMethod("pull_interval")
}

pull_interval.POSIXt <- function(x, exclude_zero = TRUE) {
  nhms <- find_interval.POSIXt(x, exclude_zero = exclude_zero)
  period <- split_period(nhms)
  structure(
    list(hour = period$hour, minute = period$minute, second = period$second),
    class = "interval"
  )
}

pull_interval.Date <- function(x, exclude_zero = TRUE) {
  ndays <- find_interval.Date(x, exclude_zero = exclude_zero)
  structure(list(day = ndays), class = "interval")
}

pull_interval.yearmth <- function(x, exclude_zero = TRUE) {
  nmonths <- find_interval.yearmth(x, exclude_zero = exclude_zero)
  structure(list(month = nmonths), class = "interval")
}

pull_interval.yearqtr <- function(x, exclude_zero = TRUE) {
  nqtrs <- find_interval.yearqtr(x, exclude_zero = exclude_zero)
  structure(list(quarter = nqtrs), class = "interval")
}

pull_interval.year <- function(x, exclude_zero = TRUE) {
  nyrs <- find_interval.year(x, exclude_zero = exclude_zero)
  structure(list(year = nyrs), class = "interval")
}

pull_interval.numeric <- function(x, exclude_zero = TRUE) {
  nunits <- find_interval.numeric(x, exclude_zero = exclude_zero)
  structure(list(unit = nunits), class = "interval")
}

pull_interval.integer <- pull_interval.numeric

# needed for full_seq(period)
as_period <- function(x) {
  UseMethod("as_period")
}

as_period.POSIXt <- function(x) {
  int <- pull_interval.POSIXt(x, exclude_zero = FALSE)
  return(int$second + int$minute * 60 + int$hour * 60 * 60)
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
    return(year(time_x))
  } else {
    return(lubridate::date_decimal(as.numeric(time_x), tz = tz))
  }
}

time_to_date.gts <- function(x, tz = "UTC", ...) {
  time_to_date(x$bts, tz = tz, ...)
}
