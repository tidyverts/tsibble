#' Extract time interval from a vector
#'
#' Assuming regularly spaced time, the `pull_interval()` returns a list of time
#' components as the "interval" class.
#'
#' @param x A vector of `POSIXt`, `Date`, `yearmonth`, `yearquarter`, `difftime`,
#' `hms`, `integer`, `numeric`.
#'
#' @details `index_valid()` and `pull_interval()` make a tsibble extensible to 
#' support custom time index.
#' @return an "interval" class (a list) includes "year", 
#' "quarter", "month", "week", "day", "hour", "minute", "second", "millisecond",
#' "microsecond", "nanosecond", "unit".
#'
#' @rdname pull-interval
#' @export
#'
#' @examples
#' x <- seq(as.Date("2017-10-01"), as.Date("2017-10-31"), by = 3)
#' pull_interval(x)
pull_interval <- function(x) {
  if (has_length(x, 1)) {
    return(init_interval())
  }
  UseMethod("pull_interval")
}

#' @export
# Assume date is regularly spaced
pull_interval.POSIXt <- function(x) {
  fmt6 <- substring(format(x[1], "%OS6"), first = 4)
  dttm <- as.double(x)
  if (fmt6 == "000000") { # second
    nhms <- gcd_interval(dttm)
    period <- split_period(nhms)
    init_interval(
      hour = period$hour, 
      minute = period$minute, 
      second = period$second
    )
  } else if (substring(fmt6, 4) %in% c("000", "999")) { # millisecond
    nhms <- ceiling(gcd_interval(dttm) * 1e+3)
    init_interval(millisecond = nhms)
  } else { # microsecond
    dttm <- dttm * 1e+6
    nhms <- gcd_interval(dttm)
    init_interval(microsecond = nhms)
  }
}

#' @export
pull_interval.nanotime <- function(x) {
  nano <- as.numeric(x)
  int <- gcd_interval(nano) # num of nanoseconds
  init_interval(nanosecond = int)
}

#' @export
pull_interval.difftime <- function(x) {
  dttm <- as.double(x)
  nhms <- gcd_interval(dttm)
  period <- split_period(nhms)
  init_interval(
    hour = period$hour, 
    minute = period$minute, 
    second = period$second
  )
}

#' @export
pull_interval.hms <- pull_interval.difftime # for hms package

#' @export
pull_interval.Date <- function(x) {
  dttm <- as.numeric(x)
  ndays <- gcd_interval(dttm) # num of seconds
  init_interval(day = ndays)
}

#' @export
pull_interval.yearweek <- function(x) {
  wk <- units_since(x)
  nweeks <- gcd_interval(wk)
  init_interval(week = nweeks)
}

#' @export
pull_interval.yearmonth <- function(x) {
  mon <- units_since(x)
  nmonths <- gcd_interval(mon)
  init_interval(month = nmonths)
}

#' @export
pull_interval.yearmth <- function(x) {
  pull_interval(yearmonth(x))
}

#' @export
pull_interval.yearquarter <- function(x) {
  qtr <- units_since(x)
  nqtrs <- gcd_interval(qtr)
  init_interval(quarter = nqtrs)
}

#' @export
pull_interval.yearqtr <- function(x) {
  pull_interval(yearquarter(x))
}

#' @export
pull_interval.numeric <- function(x) {
  nunits <- gcd_interval(x)
  # "place our origin at 1582 if we are historically in- clined or at 1900 if 
  # we are more financially motivated." (p98, the grammar of graphics v2)
  if (min0(x) > 1581 && max0(x) < 2500) {
    init_interval(year = nunits)
  } else {
    init_interval(unit = nunits)
  }
}

#' @export
`[[.interval` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
}

#' @export
`[.interval` <- function(x, i, j, drop = FALSE) {
  NextMethod()
}

init_interval <- function(
  year = 0, quarter = 0, month = 0, week = 0, 
  day = 0, hour = 0, minute = 0, second = 0, 
  millisecond = 0, microsecond = 0, nanosecond = 0, 
  unit = 0
) {
  structure(list(
    year = year, quarter = quarter, month = month, week = week,
    day = day, hour = hour, minute = minute, second = second,
    millisecond = millisecond, microsecond = microsecond, 
    nanosecond = nanosecond, unit = unit
  ), class = "interval")
}

#' Extract time unit from a vector
#'
#' @inheritParams pull_interval
#' @export
#' @examples
#' x <- yearmonth(seq(2016, 2018, by = 0.5))
#' time_unit(x)
time_unit <- function(x) {
  if (has_length(x, 1)) return(0L)
  int <- pull_interval(x)
  int$nanosecond + int$microsecond * 1e-6 + int$millisecond * 1e-3 +
  int$second + int$minute * 60 + int$hour * 3600 + 
  int$day + int$week + int$month + int$quarter + 
  int$year + int$unit
}

# from ts time to dates
time_to_date <- function(x, ...) {
  UseMethod("time_to_date")
}

time_to_date.ts <- function(x, tz = "UTC", ...) {
  freq <- stats::frequency(x)
  time_x <- as.numeric(stats::time(x))
  if (freq == 7) { # daily
    start_year <- trunc(time_x[1])
    as.Date(lubridate::round_date(
      lubridate::date_decimal(start_year + (time_x - start_year) * 7 / 365),
      unit = "day"
    ))
  } else if (freq == 52) { # weekly
    yearweek(lubridate::date_decimal(time_x))
  } else if (freq > 4 && freq <= 12) { # monthly
    yearmonth(time_x)
  } else if (freq > 1 && freq <= 4) { # quarterly
    yearquarter(time_x)
  } else if (freq == 1) { # yearly
    time_x
  } else {
    if (stats::end(x)[1] > 999) {
      date_x <- lubridate::date_decimal(time_x, tz = tz)
      lubridate::round_date(date_x, unit = "seconds")
    } else {
      time_x
    }
  }
}

time_to_date.gts <- function(x, tz = "UTC", ...) {
  time_to_date(x$bts, tz = tz, ...)
}
