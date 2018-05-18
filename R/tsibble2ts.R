#' Coerce a tsibble to a time series
#'
#' @param x A `tbl_ts` object.
#' @param value A measured variable of interest to be spread over columns, if
#' multiple measures.
#' @param frequency A smart frequency with the default `NULL`. If set, the 
#' preferred frequency is passed to `ts()`.
#' @param fill A value replaces missing values.
#' @param ... Ignored for the function.
#'
#' @return A `ts` object.
#' @export
#'
#' @examples
#' # a monthly series ----
#' x1 <- as_tsibble(AirPassengers)
#' as.ts(x1)
#' 
#' # equally spaced over trading days, not smart enough to guess frequency ----
#' x2 <- as_tsibble(EuStockMarkets)
#' head(as.ts(x2, frequency = 260))
as.ts.tbl_ts <- function(x, value, frequency = NULL, fill = NA, ...) {
  value <- enquo(value)
  key_vars <- key(x)
  if (is_empty(key_vars)) {
    return(finalise_ts(x, index = index(x), frequency = frequency))
  }
  if (any(is_nest(key_vars)) || length(key_vars) > 1) {
    abort("`as.ts()` can't deal with nested or crossed keys.")
  }
  mvars <- measured_vars(x)
  str_val <- paste_comma(surround(mvars, "`"))
  if (quo_is_missing(value)) {
    if (is_false(has_length(mvars, 1))) {
      abort(sprintf("Can't determine the `value`: %s.", str_val))
    }
    value_var <- mvars
  } else {
    value_var <- tidyselect::vars_pull(names(x), !! value)
    if (is_false(value_var %in% mvars)) {
      abort(sprintf("`value` must be one of them: %s.", str_val))
    }
  }
  idx <- index(x)
  mat_ts <- x %>% 
    arrange(!!! key_vars, !! idx) %>% 
    select(!! idx, !!! key_vars, !! value_var) %>% 
    spread(key = key_vars(x), value = value_var, fill = fill)
  finalise_ts(mat_ts, index = idx, frequency = frequency)
}

finalise_ts <- function(data, index, frequency = NULL) {
  idx_time <- time(dplyr::pull(data, !! index))
  out <- select(data, - !! index, .drop = TRUE)
  if (NCOL(out) == 1) {
    out <- out[[1]]
  }
  if (is.null(frequency)) {
    frequency <- stats::frequency(idx_time)
  }
  stats::ts(out, stats::start(idx_time), frequency = frequency)
}

#' @importFrom stats as.ts tsp<- time
#' @export
time.yearweek <- function(x, ...) {
  freq <- guess_frequency(x)
  y <- lubridate::decimal_date(x)
  stats::ts(y, start = min0(y), frequency = freq)
}

#' @export
time.yearmonth <- function(x, ...) {
  freq <- guess_frequency(x)
  y <- lubridate::year(x) + (lubridate::month(x) - 1) / freq
  stats::ts(y, start = min0(y), frequency = freq)
}

#' @export
time.yearquarter <- function(x, ...) {
  freq <- guess_frequency(x)
  y <- lubridate::year(x) + (lubridate::quarter(x) - 1) / freq
  stats::ts(y, start = min0(y), frequency = freq)
}

#' @export
time.numeric <- function(x, ...) {
  stats::ts(x, start = min0(x), frequency = 1)
}

#' @export
time.Date <- function(x, frequency = NULL, ...) {
  if (is.null(frequency)) {
    frequency <- guess_frequency(x)
  }
  y <- lubridate::decimal_date(x)
  stats::ts(x, start = min0(y), frequency = frequency)
}

#' @export
time.POSIXt <- function(x, frequency = NULL, ...) {
  if (is.null(frequency)) {
    frequency <- guess_frequency(x)
  }
  y <- lubridate::decimal_date(x)
  stats::ts(x, start = min0(y), frequency = frequency)
}

#' Guess a time frequency from other index objects
#'
#' A possible frequency passed to the `ts()` function
#'
#' @param x An index object including "yearmonth", "yearquarter", "Date" and others.
#'
#' @details If a series of observations are collected more frequently than 
#' weekly, it is more likely to have multiple seasonalities. This function
#' returns a frequency value at its nearest ceiling time resolution. For example, 
#' hourly data would have daily, weekly and annual frequencies of 24, 168 and 8766
#' respectively, and hence it gives 24.
#'
#' @references <https://robjhyndman.com/hyndsight/seasonal-periods/>
#'
#' @export
#'
#' @examples
#' guess_frequency(yearquarter(seq(2016, 2018, by = 1 / 4)))
#' guess_frequency(yearmonth(seq(2016, 2018, by = 1 / 12)))
#' guess_frequency(seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by = 1))
#' guess_frequency(seq(
#'   as.POSIXct("2017-01-01 00:00"), as.POSIXct("2017-01-10 23:00"), 
#'   by = "1 hour"
#' ))
guess_frequency <- function(x) {
  UseMethod("guess_frequency")
}

#' @export
guess_frequency.yearweek <- function(x) {
  52 / pull_interval(x)$month
}

#' @export
guess_frequency.yearmonth <- function(x) {
  12 / pull_interval(x)$month
}

#' @export
guess_frequency.yearquarter <- function(x) {
  4 / pull_interval(x)$quarter
}

#' @export
guess_frequency.Date <- function(x) {
  7 / pull_interval(x)$day
}

#' @export
guess_frequency.POSIXt <- function(x) {
  int <- pull_interval(x)
  number <- int$hour + int$minute / 60 + int$second / 3600
  if (number > 1 / 60) {
    return(24 / number)
  } else if (number > 1 / 3600 && number <= 1 / 60) {
    return(3600 * number)
  } else {
    3600 * 60 * number
  }
}
