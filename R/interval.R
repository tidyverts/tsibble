#' Pull time interval from a vector and return a class of "interval"
#'
#' S3 method to pull time interval from various classes, assuming of regularly
#' spaced time. It finds a minimal time span from `x`.
#'
#' @param x A vector of `POSIXt`, `Date`, `yearmonth`, `yearquarter`, `difftime`,
#' `hms`, `integer`, `numeric`.
#' @param duplicated `TRUE` removes the duplicated elements of `x` and compute
#' the minimal time span.
#'
#' @details As an S3 method, the `pull_interval()` makes a tsibble extensible to
#' support custom time index.
#' @return An "interval" class (a list) includes "year", "quarter", "month",
#' "week", "day", "hour", "minute", "second", "unit", and other self-defined 
#' interval.
#' 
#' @rdname pull-interval
#' @export
#'
#' @examples
#' x <- seq(as.Date("2017-10-01"), as.Date("2017-10-31"), by = 3)
#' pull_interval(x, duplicated = FALSE)
pull_interval <- function(x, duplicated = TRUE) {
  UseMethod("pull_interval")
}

#' @export
# Assume date is regularly spaced
pull_interval.POSIXt <- function(x, duplicated = TRUE) {
  dttm <- as.numeric(x)
  nhms <- min_interval(dttm, duplicated = duplicated) # num of seconds
  period <- split_period(nhms)
  structure(
    list(hour = period$hour, minute = period$minute, second = period$second),
    class = "interval"
  )
}

#' @export
pull_interval.difftime <- pull_interval.POSIXt

#' @export
pull_interval.hms <- pull_interval.difftime # for hms package

#' @export
pull_interval.Date <- function(x, duplicated = TRUE) {
  dttm <- as.numeric(x)
  ndays <- min_interval(dttm, duplicated = duplicated) # num of seconds
  structure(list(day = ndays), class = "interval")
}

#' @export
pull_interval.yearmonth <- function(x, duplicated = TRUE) {
  mon <- year(x) + (lubridate::month(x) - 1) / 12
  nmonths <- ceiling(min_interval(mon, duplicated = duplicated) * 12)
  structure(list(month = nmonths), class = "interval")
}

#' @export
pull_interval.yearquarter <- function(x, duplicated = TRUE) {
  qtr <- year(x) + (lubridate::quarter(x) - 1) / 4
  nqtrs <- ceiling(min_interval(qtr, duplicated = duplicated) * 4)
  structure(list(quarter = nqtrs), class = "interval")
}

#' @export
pull_interval.integer <- function(x, duplicated = TRUE) {
  nyrs <- as.integer(min_interval(x, duplicated = duplicated))
  structure(list(year = nyrs), class = "interval")
}

#' @export
pull_interval.numeric <- function(x, duplicated = TRUE) {
  nunits <- min_interval(x, duplicated = duplicated)
  structure(list(unit = nunits), class = "interval")
}

as_period <- function(x) {
  UseMethod("as_period")
}

as_period.POSIXt <- function(x) {
  int <- pull_interval(x)
  return(int$second + int$minute * 60 + int$hour * 60 * 60)
}

as_period.numeric <- function(x) {
  int <- pull_interval(x)
  return(int$unit)
}

as_period.integer <- function(x) {
  int <- pull_interval(x)
  return(int$year)
}

as_period.Date <- function(x) {
  int <- pull_interval(x)
  return(int$day)
}

as_period.yearmonth <- function(x) {
  int <- pull_interval(x)
  return(int$month)
}

as_period.yearquarter <- function(x) {
  int <- pull_interval(x)
  return(int$quarter)
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
    date_x <- lubridate::date_decimal(time_x, tz = tz)
    return(lubridate::round_date(date_x, unit = "seconds"))
  }
}

time_to_date.gts <- function(x, tz = "UTC", ...) {
  time_to_date(x$bts, tz = tz, ...)
}
