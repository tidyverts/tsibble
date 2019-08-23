#' Pull time interval from a vector
#'
#' @description
#' \lifecycle{stable}
#'
#' Assuming regularly spaced time, the `interval_pull()` returns a list of time
#' components as the "interval" class.
#'
#' @param x A vector of `POSIXct`, `Date`, `yearweek`, `yearmonth`, `yearquarter`,
#' `difftime`/`hms`, `ordered`, `integer`, `numeric`, and `nanotime`.
#'
#' @details Extend tsibble to support custom time indexes by defining S3 generics
#' `index_valid()` and `interval_pull()` for them.
#' @return an "interval" class (a list) includes "year",
#' "quarter", "month", "week", "day", "hour", "minute", "second", "millisecond",
#' "microsecond", "nanosecond", "unit".
#'
#' @rdname interval-pull
#' @export
#'
#' @examples
#' x <- seq(as.Date("2017-10-01"), as.Date("2017-10-31"), by = 3)
#' interval_pull(x)
interval_pull <- function(x) {
  UseMethod("interval_pull")
}

#' @export
interval_pull.default <- function(x) {
  dont_know(x, "interval_pull()")
}

#' @export
# Assume date is regularly spaced
interval_pull.POSIXt <- function(x) {
  dttm <- as.double(x)
  nhms <- gcd_interval(dttm)
  period <- split_period(nhms)
  init_interval(
    hour = period$hour,
    minute = period$minute,
    second = period$second %/% 1,
    millisecond = period$second %% 1 %/% 1e-3,
    microsecond = period$second %% 1 %/% 1e-6 %% 1e+3
  )
}

#' @export
interval_pull.nanotime <- function(x) {
  nano <- as.numeric(x)
  int <- gcd_interval(nano) # num of nanoseconds
  init_interval(nanosecond = int)
}

#' @export
interval_pull.difftime <- function(x) {
  t_units <- units(x)
  if (t_units == "weeks") {
    nweeks <- gcd_interval(unclass(x))
    init_interval(week = nweeks)
  } else if (t_units == "months") {
    nmths <- gcd_interval(unclass(x))
    init_interval(month = nmths)
  } else if (t_units == "quarters") {
    nqtrs <- gcd_interval(unclass(x))
    init_interval(quarter = nqtrs)
  } else {
    dttm <- as.double(x, units = "secs")
    nhms <- gcd_interval(dttm)
    period <- split_period(nhms)
    init_interval(
      day = period$day,
      hour = period$hour,
      minute = period$minute,
      second = period$second
    )
  }
}

#' @export
interval_pull.hms <- function(x) { # for hms package
  dttm <- as.double(x)
  nhms <- gcd_interval(dttm)
  period <- split_period(nhms)
  init_interval(
    hour = period$hour + period$day * 24,
    minute = period$minute,
    second = period$second %/% 1,
    millisecond = period$second %% 1 %/% 1e-3,
    microsecond = period$second %% 1 %/% 1e-6 %% 1e+3
  )
}

#' @export
interval_pull.Date <- function(x) {
  dttm <- as.numeric(x)
  ndays <- gcd_interval(dttm) # num of seconds
  init_interval(day = ndays)
}

#' @export
interval_pull.yearweek <- function(x) {
  wk <- units_since(x)
  nweeks <- gcd_interval(wk)
  init_interval(week = nweeks)
}

#' @export
interval_pull.yearmonth <- function(x) {
  mon <- units_since(x)
  nmonths <- gcd_interval(mon)
  init_interval(month = nmonths)
}

#' @export
interval_pull.yearmon <- function(x) {
  interval_pull(yearmonth(x))
}

#' @export
interval_pull.yearquarter <- function(x) {
  qtr <- units_since(x)
  nqtrs <- gcd_interval(qtr)
  init_interval(quarter = nqtrs)
}

#' @export
interval_pull.yearqtr <- function(x) {
  interval_pull(yearquarter(x))
}

#' @export
interval_pull.numeric <- function(x) {
  nunits <- gcd_interval(x)
  # "place our origin at 1582 if we are historically inclined or at 1900 if
  # we are more financially motivated." (p98, the grammar of graphics v2)
  if (is_empty(x)) {
    init_interval(unit = nunits)
  } else if (min0(x) > 1581 && max0(x) < 2500) {
    init_interval(year = nunits)
  } else {
    init_interval(unit = nunits)
  }
}

#' @export
interval_pull.ordered <- function(x) {
  interval_pull(as.integer(x))
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
#' new_interval(NULL) # irregular interval
#' new_interval() # unknown interval
new_interval <- function(...) {
  args <- list2(...)
  if (has_length(args, 1) && is_null(args[[1]])) return(irregular())

  if (is_false(all(map_lgl(args, ~ has_length(., 1))))) {
    abort("Only accepts one input for each unit, not multiple.")
  }
  names_args <- names(args)
  names_unit <- fn_fmls_names(init_interval)
  pidx <- pmatch(names_args, names_unit)
  if (anyNA(pidx)) {
    x_units <- paste(names_args[is.na(pidx)], collapse = ", ")
    abort(sprintf("Invalid unit name: %s.", x_units))
  }
  # names(args) <- names_unit[pidx]
  eval_tidy(call2("init_interval", !!!args))
}

init_interval <- function(year = 0, quarter = 0, month = 0, week = 0,
                          day = 0, hour = 0, minute = 0, second = 0,
                          millisecond = 0, microsecond = 0, nanosecond = 0,
                          unit = 0) {
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
  for(y in x) if(y) return(FALSE)
  !is_empty(x)
}

#' @export
print.interval <- function(x, digits = NULL, ...) {
  cat_line(format(x, digits = digits, ...))
  invisible(x)
}

#' @export
format.interval <- function(x, digits = NULL, ...) {
  if (is_empty(x)) return("!")

  not_zero <- !map_lgl(x, function(x) x == 0)
  # if output contains all the zeros
  if (sum(not_zero) == 0) return("?")

  x <- translate_interval(x)
  output <- x[not_zero]
  paste0(output, names(output), collapse = " ")
}

translate_interval <- function(x) {
  names_unit <- fn_fmls_names(init_interval)
  set_names(
    x[names_unit],
    c(
      "Y", "Q", "M", "W", "D", "h", "m", "s", "ms",
      ifelse(is_utf8_output(), "\U00B5s", "us"), "ns", ""
    )
  )
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
  x[["microsecond"]] <- x[["microsecond"]] * 1e-6
  x[["millisecond"]] <- x[["millisecond"]] * 1e-3
  x[["minute"]] <- x[["minute"]] * 60
  x[["hour"]] <- x[["hour"]] * 3600
  sum(vec_c(!!!x))
}

# from ts time to dates
time_to_date <- function(x, ...) {
  UseMethod("time_to_date")
}

time_to_date.ts <- function(x, tz = "UTC", ...) {
  freq <- frequency(x)
  time_x <- round(as.numeric(time(x)), digits = 6) # floating
  if (freq == 52) {
    warn("Expected frequency of weekly data: 365.25 / 7 (approx 52.18), not  52.")
  }
  if (freq == 7) { # daily
    start_year <- trunc(time_x[1])
    as.Date(round_date(
      date_decimal(start_year + (time_x - start_year) * 7 / 365),
      unit = "day"
    ))
  } else if (round(freq, 2) == 52.18) { # weekly
    yearweek(date_decimal(time_x))
  } else if (freq > 4 && freq <= 12) { # monthly
    yearmonth(time_x)
  } else if (freq > 1 && freq <= 4) { # quarterly
    yearquarter(time_x)
  } else if (freq == 1) { # yearly
    time_x
  } else {
    if (end(x)[1] > 1581) {
      date_x <- date_decimal(time_x, tz = tz)
      round_date(date_x, unit = "seconds")
    } else {
      time_x
    }
  }
}

time_to_date.gts <- function(x, tz = "UTC", ...) {
  time_to_date(x$bts, tz = tz, ...)
}

# regular time interval is obtained from the greatest common divisor of positive
# time distances.
gcd_interval <- function(x) {
  if (length(x) < 2) { # only one time index
    0
  } else {
    unique_x <- vec_unique(round(abs(diff(x)), digits = 6))
    gcd_vector(unique_x)
  }
}

gcd <- function(a, b) {
  if (isTRUE(all.equal(b, 0))) a else gcd(b, a %% b)
}

gcd_vector <- function(x) Reduce(gcd, x)

split_period <- function(x) {
  output <- seconds_to_period(x)
  list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  )
}

has_tz <- function(x) {
  tz <- attr(x, "tzone")[[1]]
  !(is_null(tz) && !inherits(x, "POSIXct"))
}
