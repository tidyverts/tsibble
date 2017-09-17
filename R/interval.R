# Number of time units
gen_interval <- function(date) {
  UseMethod("gen_interval")
}

gen_interval.default <- function(date) {
  min_interval(date) # num of years
}

gen_interval.POSIXt <- function(date) {
  dttm <- as.numeric(date)
  min_interval(dttm) # num of seconds
}

gen_interval.Date <- function(date) {
  date <- as.numeric(date)
  min_interval(date) # num of days
}

gen_interval.yearmon <- function(date) {
  # num of months
  mon <- as.numeric(date)
  ceiling(min_interval(mon) * 12)
}

gen_interval.yearqtr <- function(date) {
  # num of quarters
  qtr <- as.numeric(date)
  ceiling(min_interval(qtr) * 4)
}

# Assume date is regularly spaced
# R6Class to manage tsibble interval, although the printing info is character.
pull_interval <- function(date) {
  UseMethod("pull_interval")
}

pull_interval.POSIXt <- function(date) {
  nhms <- gen_interval.POSIXt(date)
  period <- period2list(nhms)
  structure(
    list(hour = period$hour, minute = period$minute, second = period$second),
    class = c("hms", "frequency")
  )
}

pull_interval.Date <- function(date) {
  ndays <- gen_interval.Date(date)
  structure(list(day = ndays), class = c("day", "frequency"))
}

pull_interval.yearmon <- function(date) {
  nmonths <- gen_interval.yearmon(date)
  structure(list(month = nmonths), class = c("month", "frequency"))
}

pull_interval.yearqtr <- function(date) {
  nqtrs <- gen_interval.yearqtr(date)
  structure(list(quarter = nqtrs), class = c("quarter", "frequency"))
}

pull_interval.default <- function(date) {
  nyrs <- gen_interval.default(date)
  structure(list(year = nyrs), class = c("year", "frequency"))
}

display_int <- function(x) {
  not_zero <- !purrr::map_lgl(x, function(x) x == 0)
  output <- x[not_zero]
  paste0(rlang::flatten_dbl(output), toupper(names(output)))
}

## helper function
period2list <- function(x) {
  output <- lubridate::seconds_to_period(x)
  list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  )
} 

min_interval <- function(date) {
  if (has_length(date, 1)) { # only one time index
    return(NA_integer_)
  }
  min(abs(diff(as.numeric(date), na.rm = TRUE)))
}

# from ts time to dates
time2date <- function(x, ...) {
  UseMethod("time2date")
}

time2date.ts <- function(x, tz = "UTC", ...) {
  freq <- frequency(x)
  time_x <- time(x)
  if (freq == 12) { # monthly
    return(as.yearmon(time_x))
  } else if (freq == 4) { # quarterly
    return(as.yearqtr(time_x))
  } else if (freq == 1) { # yearly
    return(as.numeric(time_x))
  } else {
    return(date_decimal(as.numeric(time_x), tz = tz))
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
