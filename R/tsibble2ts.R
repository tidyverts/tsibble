#' Coerce a tsibble to a time series
#'
#' \lifecycle{stable}
#'
#' @param x A `tbl_ts` object.
#' @param value A measured variable of interest to be spread over columns, if
#' multiple measures.
#' @param frequency A smart frequency with the default `NULL`. If set, the
#' preferred frequency is passed to `ts()`.
#' @param fill A value to replace missing values.
#' @param ... Ignored for the function.
#'
#' @return A `ts` object.
#' @export
#'
#' @examples
#' # a monthly series
#' x1 <- as_tsibble(AirPassengers)
#' as.ts(x1)
as.ts.tbl_ts <- function(x, value, frequency = NULL, fill = NA_real_, ...) {
  stopifnot(!is_null(fill))
  value <- enquo(value)
  key_vars <- key(x)
  if (length(key_vars) > 1 && n_keys(x) > 1) {
    abort("Can't proceed with the key of multiple variables.")
  }
  mvars <- measured_vars(x)
  str_val <- comma(backticks(mvars))
  if (quo_is_missing(value)) {
    if (is_false(has_length(mvars, 1) || is_empty(key_vars))) {
      abort(sprintf("Can't determine column `value`: %s.", str_val))
    }
    value_var <- mvars
  } else {
    value_var <- vars_pull(names(x), !!value)
    if (is_false(value_var %in% mvars)) {
      abort(sprintf("Column `value` must be one of them: %s.", str_val))
    }
  }
  idx <- index(x)
  vars_fill <- vec_rep(fill, length(value_var))
  vars_fill <- set_names(vars_fill, nm = value_var)
  tsbl_sel <- fill_gaps(
    select(x, !!idx, !!!key_vars, !!value_var),
    !!!vars_fill, .full = TRUE)
  pivot_wider_ts(tsbl_sel, frequency = frequency)
}

pivot_wider_ts <- function(data, frequency = NULL) {
  index <- index_var(data)
  df_rows <- data[[index]]
  idx_time <- time(df_rows)
  rows <- vec_unique(df_rows)
  key_rows <- key_rows(data)
  mvars <- measured_vars(data)
  if (has_length(mvars, 1)) {
    res <- data[[mvars]]
  } else {
    res <- data[mvars]
  }
  nseries <- vec_size(key_rows)
  if (nseries > 1) {
    res <- matrix(res, ncol = nseries)
    colnames(res) <- vec_unique(data[[key_vars(data)]])
  }
  if (is_null(frequency)) {
    frequency <- frequency(idx_time)
  }
  ts(res, start(idx_time), frequency = frequency)
}

#' @export
time.yearweek <- function(x, ...) {
  freq <- guess_frequency(x)
  y <- decimal_date(x)
  ts(y, start = min0(y), frequency = freq)
}

#' @export
time.yearmonth <- function(x, ...) {
  freq <- guess_frequency(x)
  y <- year(x) + (month(x) - 1) / freq
  ts(y, start = min0(y), frequency = freq)
}

#' @export
time.yearquarter <- function(x, ...) {
  freq <- guess_frequency(x)
  y <- year(x) + (quarter(x) - 1) / freq
  ts(y, start = min0(y), frequency = freq)
}

#' @export
time.numeric <- function(x, ...) {
  ts(x, start = min0(x), frequency = 1)
}

#' @export
time.Date <- function(x, frequency = NULL, ...) {
  if (is.null(frequency)) {
    frequency <- guess_frequency(x)
  }
  y <- decimal_date(x)
  ts(x, start = min0(y), frequency = frequency)
}

#' @export
time.POSIXt <- function(x, frequency = NULL, ...) {
  if (is.null(frequency)) {
    frequency <- guess_frequency(x)
  }
  y <- decimal_date(x)
  ts(x, start = min0(y), frequency = frequency)
}

#' Guess a time frequency from other index objects
#'
#' @description
#' \lifecycle{stable}
#'
#' A possible frequency passed to the `ts()` function
#'
#' @param x An index object including "yearmonth", "yearquarter", "Date" and others.
#'
#' @details If a series of observations are collected more frequently than
#' weekly, it is more likely to have multiple seasonalities. This function
#' returns a frequency value at its smallest. For example, hourly data would
#' have daily, weekly and annual frequencies of 24, 168 and 8766 respectively,
#' and hence it gives 24.
#'
#' @references <https://robjhyndman.com/hyndsight/seasonal-periods/>
#'
#' @export
#'
#' @examples
#' guess_frequency(yearquarter("2016 Q1") + 0:7)
#' guess_frequency(yearmonth("2016 Jan") + 0:23)
#' guess_frequency(seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by = 1))
#' guess_frequency(seq(
#'   as.POSIXct("2017-01-01 00:00"), as.POSIXct("2017-01-10 23:00"),
#'   by = "1 hour"
#' ))
guess_frequency <- function(x) {
  UseMethod("guess_frequency")
}

#' @export
guess_frequency.numeric <- function(x) {
  if (has_length(x, 1)) {
    1
  } else {
    gcd_interval(x)
  }
}

#' @export
guess_frequency.yearweek <- function(x) {
  if (has_length(x, 1)) {
    52.18
  } else {
    round(365.25 / 7 / interval_pull(x)$week, 2)
  }
}

#' @export
guess_frequency.yearmonth <- function(x) {
  if (has_length(x, 1)) {
    12
  } else {
    12 / interval_pull(x)$month
  }
}

#' @export
guess_frequency.yearmon <- guess_frequency.yearmonth

#' @export
guess_frequency.yearquarter <- function(x) {
  if (has_length(x, 1)) {
    4
  } else {
    4 / interval_pull(x)$quarter
  }
}

#' @export
guess_frequency.yearqtr <- guess_frequency.yearquarter

#' @export
guess_frequency.Date <- function(x) {
  if (has_length(x, 1)) {
    7
  } else {
    7 / interval_pull(x)$day
  }
}

#' @export
guess_frequency.POSIXt <- function(x) {
  int <- interval_pull(x)
  number <- int$hour + int$minute / 60 + int$second / 3600
  if (has_length(x, 1)) {
    1
  } else if (number > 1 / 60) {
    24 / number
  } else if (number > 1 / 3600 && number <= 1 / 60) {
    3600 * number
  } else {
    3600 * 60 * number
  }
}

#' @export
frequency.tbl_ts <- function(x, ...) {
  abort_if_irregular(x)
  guess_frequency(x[[index_var(x)]])
}
