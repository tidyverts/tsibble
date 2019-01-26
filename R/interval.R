#' Pull time interval from a vector
#'
#' Assuming regularly spaced time, the `pull_interval()` returns a list of time
#' components as the "interval" class.
#'
#' @param x A vector of `POSIXt`, `Date`, `yearmonth`, `yearquarter`, `difftime`,
#' `hms`, `ordered`, `integer`, `numeric`.
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
  if (has_length(x, 1L) || has_length(x, 0L)) {
    return(init_interval())
  }
  UseMethod("pull_interval")
}

#' @export
pull_interval.default <- function(x) {
  init_interval()
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
    dttm <- round(dttm * 1e+3)
    nhms <- gcd_interval(dttm)
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
pull_interval.yearmon <- function(x) {
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
  # "place our origin at 1582 if we are historically inclined or at 1900 if 
  # we are more financially motivated." (p98, the grammar of graphics v2)
  if (min0(x) > 1581 && max0(x) < 2500) {
    init_interval(year = nunits)
  } else {
    init_interval(unit = nunits)
  }
}

#' @export
pull_interval.ordered <- function(x) {
  pull_interval(as.integer(x))
}

#' @export
`[[.interval` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
}

#' @export
`[.interval` <- function(x, i, j, drop = FALSE) {
  NextMethod()
}

#' Create a time interval
#'
#' `new_interval()` creates an interval object with the specified values.
#'
#' @param ... A list of time units to be included in the interval and their
#' amounts. "year", "quarter", "month", "week", "day", "hour", "minute", "second", 
#' "millisecond", "microsecond", "nanosecond", "unit" are supported.
#'
#' @return an "interval" class
#' @export
#' @examples
#' new_interval(hour = 1, minute = 30)
new_interval <- function(...) {
  args <- list2(...)
  if (is_false(all(map_lgl(args, ~ has_length(., 1))))) {
    abort("Only accepts one input for each unit, not `NULL` or multiple.")
  }
  names_args <- names(args)
  names_unit <- fn_fmls_names(init_interval)
  pidx <- pmatch(names_args, names_unit)
  if (anyNA(pidx)) {
    x_units <- paste(names_args[is.na(pidx)], collapse = ", ")
    abort(sprintf("Invalid unit name: %s.", x_units))
  }
  # names(args) <- names_unit[pidx]
  eval_tidy(call2("init_interval", !!! args))
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

irregular <- function() {
  structure(list(), class = "interval")
}

unknown_interval <- function(x) {
  no_zeros <- !map_lgl(x, function(x) x == 0)
  sum(no_zeros) == 0
}

#' Extract time unit from a vector
#'
#' @param x An interval.
#' @export
#' @keywords internal
time_unit <- function(x) {
  if (is_false(inherits(x, "interval"))) {
    abort("Must be class interval.")
  }
  x[["microsecond"]]  <- x[["microsecond"]] * 1e-6
  x[["millisecond"]] <- x[["millisecond"]] * 1e-3
  x[["minute"]] <- x[["minute"]] * 60
  x[["hour"]] <- x[["hour"]] * 3600
  purrr::reduce(x, `+`)
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
    if (stats::end(x)[1] > 1581) {
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
