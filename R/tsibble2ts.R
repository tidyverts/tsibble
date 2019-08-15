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
#'
#' # equally spaced over trading days, not smart enough to guess frequency
#' x2 <- as_tsibble(EuStockMarkets)
#' head(as.ts(x2, frequency = 260))
as.ts.tbl_ts <- function(x, value, frequency = NULL, fill = NA_real_, ...) {
  value <- enquo(value)
  key_vars <- key(x)
  if (length(key_vars) > 1) {
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
  tsbl_sel <- select_tsibble(x, !!idx, !!!key_vars, !!value_var,
    validate = FALSE)
  if (is_empty(key_vars)) {
    if (!is_ordered(x)) {
      tsbl_sel <- arrange(tsbl_sel, !!idx)
    }
    finalise_ts(tsbl_sel, index = idx, frequency = frequency)
  } else {
    mat_ts <- pivot_wider_ts(tsbl_sel, fill = fill)
    finalise_ts(mat_ts, index = idx, frequency = frequency)
  }
}

pivot_wider_ts <- function(data, fill = NA_real_) {
  stopifnot(!is_null(fill))
  index <- index_var(data)
  df_rows <- data[[index]]
  rows <- vec_sort(vec_unique(df_rows))
  row_idx <- vec_match(df_rows, rows)
  key_rows <- key_rows(data)
  col_idx <- rep(seq_along(key_rows), times = map_int(key_rows, length))
  col_idx <- vec_slice(col_idx, vec_c(!!!key_rows))
  val_idx <- new_data_frame(list(row = row_idx, col = col_idx))
  values <- data[[measured_vars(data)]]
  nrow <- vec_size(rows)
  ncol <- vec_size(key_rows)
  vec <- vec_init(fill, n = nrow * ncol)
  vec[] <- fill
  vec_slice(vec, val_idx$row + nrow * (val_idx$col - 1L)) <- values
  res <- set_names(vec_init(list(), ncol), key_data(data)[[1]])
  for (i in 1:ncol) {
    res[[i]] <- vec[((i - 1) * nrow + 1):(i * nrow)]
  }
  vec_cbind(!!index := rows, !!!res)
}

finalise_ts <- function(data, index, frequency = NULL) {
  idx_time <- time(pull(data, !!index))
  out <- select(as_tibble(data), -!!index)
  if (NCOL(out) == 1) {
    out <- out[[1]]
  }
  if (is_null(frequency)) {
    frequency <- frequency(idx_time)
  }
  ts(out, start(idx_time), frequency = frequency)
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
  not_regular(x)
  guess_frequency(x[[index_var(x)]])
}
