#' Pull time interval from a vector
#'
#' @description
#' \lifecycle{stable}
#'
#' Assuming regularly spaced time, the `interval_pull()` returns a list of time
#' components as the "interval" class.
#'
#' @param x A vector of index-like class.
#'
#' @details Extend tsibble to support custom time indexes by defining S3 generics
#' `index_valid()` and `interval_pull()` for them.
#' @return An "interval" class (a list) includes "year",
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
    millisecond = (period$second %% 1 * 1e3) %/% 1,
    microsecond = (period$second %% 1 * 1e3) %% 1 * 1e3
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
interval_pull.Period <- function(x) {
  second_int <- gcd_interval(x$second)
  new_interval(
    year = gcd_interval(x$year),
    month = gcd_interval(x$month),
    day = gcd_interval(x$day),
    hour = gcd_interval(x$hour),
    minute = gcd_interval(x$minute),
    second = second_int %/% 1,
    millisecond = round(second_int %% 1 %/% 1e-3),
    microsecond = round(second_int %% 1 %/% 1e-6 %% 1e+3)
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
  wk <- as.double(x)
  nweeks <- gcd_interval(wk)
  new_interval(week = nweeks)
}

#' @export
interval_pull.yearmonth <- function(x) {
  mon <- as.double(x)
  nmonths <- gcd_interval(mon)
  new_interval(month = nmonths)
}

#' @export
interval_pull.yearquarter <- function(x) {
  qtr <- as.double(x)
  nqtrs <- gcd_interval(qtr)
  new_interval(quarter = nqtrs)
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
#' * `new_interval()` creates an interval object.
#' * `gcd_interval()` computes the greatest common divisor for the difference
#' of numerics.
#' * `is_regular_interval()` checks if the interval is regular.
#'
#' @param ... A set of name-value pairs to specify default interval units: "year",
#' "quarter", "month", "week", "day", "hour", "minute", "second", "millisecond",
#' "microsecond", "nanosecond", "unit".
#' @param .regular Logical. `FALSE` gives an irregular interval, and will ignore
#' the `...` argument.
#' @param .others A list name-value pairs that are not included in the `...`,
#' to allow custom interval.
#' @param x A vector of numerics.
#'
#' @return an "interval" class
#' @rdname new-interval
#' @export
#' @examples
#' (x <- new_interval(hour = 1, minute = 30))
#' (y <- new_interval(.regular = FALSE)) # irregular interval
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
  default <- eval_bare(call2("default_interval", !!!default))
  out <- dots_list(!!!default, !!!.others, .homonyms = "error")
  if (!(all(map_lgl(out, function(x) has_length(x, 1))))) {
    abort("Only accepts one input for each argument, not empty or multiple.")
  }
  new_rcrd(fields = out, .regular = .regular, class = "interval")
}

#' @param x An interval.
#' @rdname new-interval
#' @export
#' @examples
#' is_regular_interval(x)
#' is_regular_interval(y)
is_regular_interval <- function(x) {
  abort_not_interval(x)
  x %@% ".regular"
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
  is_regular_interval(x) && sum(vec_c(!!!vec_data(x))) == 0
}

#' @export
`[[.interval` <- function(x, i, ...) {
  field(x, i)
}

#' @export
`$.interval` <- `[[.interval`

#' @export
format.interval <- function(x, ...) {
  if (!is_regular_interval(x)) return("!")
  if (unknown_interval(x)) return("?")

  n <- n_fields(x)
  micro <- ifelse(is_utf8_output(), "\U00B5s", "us")
  defaults <- vec_c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms", micro, "ns", "")
  n_defaults <- vec_size(defaults)
  misc <- if (n > n_defaults) vec_slice(fields(x), (n_defaults + 1):n) else NULL
  fmt_names <- vec_c(defaults, misc)
  val <- vec_c(!!!vec_data(x))
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
  abort_not_interval(x)
  x <- vec_data(x)
  x[["microsecond"]] <- x[["microsecond"]] * 1e-6
  x[["millisecond"]] <- x[["millisecond"]] * 1e-3
  x[["minute"]] <- x[["minute"]] * 60
  x[["hour"]] <- x[["hour"]] * 3600
  sum(vec_c(!!!x))
}

abort_not_interval <- function(x) {
  if (is_false(inherits(x, "interval"))) {
    abort("`x` must be class 'interval'.")
  }
}

#' @rdname new-interval
#' @export
#' @examples
#' gcd_interval(c(1, 3, 5, 6))
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
  !(is_null(tz) && !inherits(x, "POSIXt"))
}

#' @importFrom methods setOldClass setMethod
setOldClass(c("interval", "vctrs_rcrd", "vctrs_vctr"))

#' @importMethodsFrom lubridate as.period
setMethod("as.period", "interval", function(x, ...) {
  period(
    year = x$year, 
    month = x$quarter * 3 + x$month,
    week = x$week,
    day = x$day,
    hour = x$hour,
    minute = x$minute,
    second = x$second + x$millisecond / 1e3 + x$microsecond / 1e6 + x$nanosecond / 1e9)
})

#' @importMethodsFrom lubridate as.duration
setMethod("as.duration", "interval", function(x, ...) {
  as.duration(as.period(x))
})
