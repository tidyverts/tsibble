# Number of time units
gen_interval <- function(x, exclude_zero = TRUE) {
  UseMethod("gen_interval")
}

gen_interval.numeric <- function(x, exclude_zero = TRUE) {
  min_interval(x, exclude_zero = exclude_zero) # num of years
}

gen_interval.integer <- gen_interval.numeric

gen_interval.POSIXt <- function(x, exclude_zero = TRUE) {
  dttm <- as.numeric(x)
  min_interval(dttm, exclude_zero = exclude_zero) # num of seconds
}

gen_interval.Date <- gen_interval.POSIXt

gen_interval.yearmon <- function(x, exclude_zero = TRUE) {
  # num of months
  mon <- as.numeric(x)
  ceiling(min_interval(mon, exclude_zero = exclude_zero) * 12)
}

gen_interval.yearqtr <- function(x, exclude_zero = TRUE) {
  # num of quarters
  qtr <- as.numeric(x)
  ceiling(min_interval(qtr, exclude_zero = exclude_zero) * 4)
}

# Assume date is regularly spaced
pull_interval <- function(x, exclude_zero = TRUE) {
  UseMethod("pull_interval")
}

pull_interval.POSIXt <- function(x, exclude_zero = TRUE) {
  nhms <- gen_interval(x, exclude_zero = exclude_zero)
  period <- split_period(nhms)
  structure(
    list(hour = period$hour, minute = period$minute, second = period$second),
    class = c("hms", "interval")
  )
}

pull_interval.Date <- function(x, exclude_zero = TRUE) {
  ndays <- gen_interval(x, exclude_zero = exclude_zero)
  structure(list(day = ndays), class = c("day", "interval"))
}

pull_interval.yearmon <- function(x, exclude_zero = TRUE) {
  nmonths <- gen_interval(x, exclude_zero = exclude_zero)
  structure(list(month = nmonths), class = c("month", "interval"))
}

pull_interval.yearqtr <- function(x, exclude_zero = TRUE) {
  nqtrs <- gen_interval(x, exclude_zero = exclude_zero)
  structure(list(quarter = nqtrs), class = c("quarter", "interval"))
}

pull_interval.numeric <- function(x, exclude_zero = TRUE) {
  nyrs <- gen_interval(x, exclude_zero = exclude_zero)
  structure(list(year = nyrs), class = c("year", "interval"))
}

pull_interval.integer <- pull_interval.numeric

## helper functions
split_period <- function(x) {
  output <- lubridate::seconds_to_period(x)
  list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  )
} 

# regular time interval is obtained from the minimal time distance.
# duplicated time entries result in 0L.
# if validate = FALSE in as_tsibble, skip to check duplicated entries
min_interval <- function(x, exclude_zero = TRUE) {
  if (has_length(x, 1)) { # only one time index
    return(NA_integer_)
  }
  abs_diff <- abs(diff(as.numeric(x), na.rm = TRUE))
  if (exclude_zero) {
    return(min(abs_diff))
  } else {
    max(0L, abs_diff)
  }
}

# from ts time to dates
time2date <- function(x, ...) {
  UseMethod("time2date")
}

time2date.ts <- function(x, tz = "UTC", ...) {
  freq <- stats::frequency(x)
  time_x <- stats::time(x)
  if (freq == 12) { # monthly
    return(as.yearmon(time_x))
  } else if (freq == 4) { # quarterly
    return(as.yearqtr(time_x))
  } else if (freq == 1) { # yearly
    return(as.numeric(time_x))
  } else {
    return(lubridate::date_decimal(as.numeric(time_x), tz = tz))
  }
}

#' @export
# rep S3 methods for yearmon & yearqtr
rep.yearmon <- function(x, ...) {
  x <- NextMethod()
  structure(x, class = "yearmon")
}

#' @export
rep.yearqtr <- function(x, ...) {
  x <- NextMethod()
  structure(x, class = "yearqtr")
}
