# Number of time units
gen_interval <- function(date) {
  UseMethod("gen_interval")
}

gen_interval.default <- function(date) {
  output <- min_interval(date) # num of years
  return(output)
}

gen_interval.POSIXt <- function(date) {
  dttm <- as.numeric(date)
  output <- min_interval(dttm) # num of seconds
  return(output)
}

gen_interval.Date <- function(date) {
  date <- as.numeric(date)
  output <- min_interval(date) # num of days
  return(output)
}

gen_interval.yearmon <- function(date) {
  # num of months
  mon <- as.numeric(date)
  output <- ceiling(min_interval(mon) * 12)
  return(output)
}

gen_interval.yearqtr <- function(date) {
  # num of quarters
  qtr <- as.numeric(date)
  output <- ceiling(min_interval(qtr) * 4)
  return(output)
}

# Assume date is regularly spaced
# R6Class to manage tsibble interval, although the printing info is character.
pull_interval <- function(date) {
  UseMethod("pull_interval")
}

pull_interval.POSIXt <- function(date) {
  nhms <- gen_interval.POSIXt(date)
  period <- period2list(nhms)
  output <- structure(
    list(hour = period$hour, minute = period$minute, second = period$second),
    class = c("hms", "frequency")
  )
  return(output)
}

pull_interval.Date <- function(date) {
  ndays <- gen_interval.Date(date)
  output <- structure(list(day = ndays), class = c("day", "frequency"))
  return(output)
}

pull_interval.yearmon <- function(date) {
  nmonths <- gen_interval.yearmon(date)
  output <- structure(list(month = nmonths), class = c("month", "frequency"))
  return(output)
}

pull_interval.yearqtr <- function(date) {
  nqtrs <- gen_interval.yearqtr(date)
  output <- structure(list(quarter = nqtrs), class = c("quarter", "frequency"))
  return(output)
}

pull_interval.default <- function(date) {
  nyrs <- gen_interval.default(date)
  output <- structure(list(year = nyrs), class = c("year", "frequency"))
  return(output)
}

display_int <- function(x) {
  not_zero <- !map_lgl(x, function(x) x == 0)
  output <- x[not_zero]
  return(paste0(rlang::flatten_dbl(output), toupper(names(output))))
}

## helper function
period2list <- function(x) {
  output <- seconds_to_period(x)
  return(list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  ))
} 

min_interval <- function(date) {
  return(min(abs(diff(as.numeric(date), na.rm = TRUE))))
}

support_cls <- function() {
  return(c(
    "Date", "POSIXt", "yearmon", "yearqtr", "integer", "numeric"
  ))
}

# from ts time to dates
time2date <- function(x, ...) {
  UseMethod("time2date")
}

time2date.ts <- function(x, tz = "UTC", ...) {
  freq <- frequency(x)
  time_x <- time(x)
  if (freq == 12) { # monthly
    output <- as.yearmon(time_x)
  } else if (freq == 4) { # quarterly
    output <- as.yearqtr(time_x)
  } else if (freq == 1) { # yearly
    output <- as.numeric(time_x)
  } else {
    output <- date_decimal(as.numeric(time_x), tz = tz)
  }
  return(output)
}

#' @export
# rep S3 methods for yearmon & yearqtr
rep.yearmon <- function(x, ...) {
  x <- NextMethod()
  return(structure(x, class = "yearmon"))
}

#' @export
rep.yearqtr <- function(x, ...) {
  x <- NextMethod()
  return(structure(x, class = "yearqtr"))
}
