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
  new_interval(
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
  new_interval(nanosecond = int)
}

#' @export
interval_pull.difftime <- function(x) {
  t_units <- units(x)
  if (t_units == "weeks") {
    nweeks <- gcd_interval(unclass(x))
    new_interval(week = nweeks)
  } else if (t_units == "months") {
    nmths <- gcd_interval(unclass(x))
    new_interval(month = nmths)
  } else if (t_units == "quarters") {
    nqtrs <- gcd_interval(unclass(x))
    new_interval(quarter = nqtrs)
  } else {
    dttm <- as.double(x, units = "secs")
    nhms <- gcd_interval(dttm)
    period <- split_period(nhms)
    new_interval(
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
  new_interval(
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
  new_interval(day = ndays)
}

#' @export
interval_pull.yearweek <- function(x) {
  wk <- units_since(x)
  nweeks <- gcd_interval(wk)
  new_interval(week = nweeks)
}

#' @export
interval_pull.yearmonth <- function(x) {
  mon <- units_since(x)
  nmonths <- gcd_interval(mon)
  new_interval(month = nmonths)
}

#' @export
interval_pull.yearmon <- function(x) {
  interval_pull(yearmonth(x))
}

#' @export
interval_pull.yearquarter <- function(x) {
  qtr <- units_since(x)
  nqtrs <- gcd_interval(qtr)
  new_interval(quarter = nqtrs)
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
    new_interval(unit = nunits)
  } else if (min0(x) > 1581 && max0(x) < 2500) {
    new_interval(year = nunits)
  } else {
    new_interval(unit = nunits)
  }
}

#' @export
interval_pull.ordered <- function(x) {
  interval_pull(as.integer(x))
}

#' Interval constructor for a tsibble
#'
#' @description
#' \lifecycle{stable}
#' `new_interval()` creates an interval object.
#'
#' @param ... A set of name-value pairs to specify default interval units: "year",
#' "quarter", "month", "week", "day", "hour", "minute", "second", "millisecond",
#' "microsecond", "nanosecond", "unit".
#' @param .regular Logical. `FALSE` gives an irregular interval, and will ignore
#' the `...` argument.
#' @param .others A list name-value pairs that are not included in the `...`,
#' to allow custom interval.
#'
#' @return an "interval" class
#' @export
#' @examples
#' new_interval(hour = 1, minute = 30)
#' new_interval(.regular = FALSE) # irregular interval
#' new_interval() # unknown interval
#' new_interval(.others = list(semester = 1)) # custom interval
new_interval <- function(..., .regular = TRUE, .others = list()) {
  stopifnot(is.logical(.regular))
  stopifnot(is.list(.others))

  default <- list2(...)
  names_default <- names(default)
  names_unit <- fn_fmls_names(default_interval)
  pidx <- pmatch(names_default, names_unit)
  if (anyNA(pidx)) {
    x_units <- paste(names_default[is.na(pidx)], collapse = ", ")
    abort(sprintf("Invalid argument: %s.", x_units))
  }
  default <- eval_tidy(call2("default_interval", !!!default))
  out <- dots_list(!!!default, !!!.others, .homonyms = "error")
  if (!(all(map_lgl(out, ~ has_length(., 1))))) {
    abort("Only accepts one input for each argument, not empty or multiple.")
  }
  new_rcrd(fields = out, .regular = .regular, class = "interval")
}

default_interval <- function(year = 0, quarter = 0, month = 0, week = 0,
                             day = 0, hour = 0, minute = 0, second = 0,
                             millisecond = 0, microsecond = 0, nanosecond = 0,
                             unit = 0) {
  list(
    year = year, quarter = quarter, month = month, week = week,
    day = day, hour = hour, minute = minute, second = second,
    millisecond = millisecond, microsecond = microsecond,
    nanosecond = nanosecond, unit = unit
  )
}

irregular <- function() {
  new_interval(.regular = FALSE)
}

unknown_interval <- function(x) {
  (x %@% ".regular") && sum(vec_c(!!!unclass(x))) == 0
}

#' @export
`[[.interval` <- function(x, i, ...) {
  field(x, i)
}

#' @export
`$.interval` <- `[[.interval`

#' @export
format.interval <- function(x, ...) {
  if (is_false(x %@% ".regular")) return("!")
  if (unknown_interval(x)) return("?")

  n <- n_fields(x)
  micro <- ifelse(is_utf8_output(), "\U00B5s", "us")
  defaults <- vec_c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms", micro, "ns", "")
  n_defaults <- vec_size(defaults)
  misc <- if (n > n_defaults) vec_slice(fields(x), (n_defaults + 1):n) else NULL
  fmt_names <- vec_c(defaults, misc)
  val <- vec_c(!!!unclass(x))
  paste0(val[val != 0], fmt_names[val != 0], collapse = " ")
}

#' Time units from tsibble's "interval" class used for `seq(by = )`
#'
#' \lifecycle{experimental}
#'
#' @param x An interval.
#' @export
#' @keywords internal
default_time_units <- function(x) {
  if (is_false(inherits(x, "interval"))) {
    abort("`x` must be class 'interval'.")
  }
  x <- unclass(x)
  x[["microsecond"]] <- x[["microsecond"]] * 1e-6
  x[["millisecond"]] <- x[["millisecond"]] * 1e-3
  x[["minute"]] <- x[["minute"]] * 60
  x[["hour"]] <- x[["hour"]] * 3600
  sum(vec_c(!!!x))
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
