as.ts.tbl_ts <- function(x, value, fill = NA, ...) {
  key_vars <- key(x)
  if (is_nest(key_vars)) {
    abort("Please use as.hts() instead.")
  }
  if (length(key_vars) > 1) {
    abort("Please use as.gts() instead.")
  }
  mat_ts <- spread_tsbl(x, value = value, fill = fill)
  finalise_ts(mat_ts, index = index(x))
}

spread_tsbl <- function(data, value, fill = NA, sep = "") {
  spread_val <- measured_vars(data)
  if (is_false(has_length(spread_val, 1))) {
    str_val <- paste(spread_val, collapse = ",")
    msg <- paste0("Please specify one of the variables for 'value': ", str_val)
    abort(msg)
  }
  # ToDo: only works with a single key rather than the nested and grouped keys
  spread_key <- key(data)
  if (is.null(spread_key)) {
    return(as_tibble(data))
  }
  idx_var <- index(data)
  compact_tsbl <- data %>% 
    mutate(key = paste(!!! spread_key, sep = sep)) %>% 
    select(!! idx_var, key, spread_val)
  as_tibble(compact_tsbl) %>% 
    tidyr::spread(key = key, value = spread_val, fill = fill) %>% 
    arrange(!! idx_var)
}

finalise_ts <- function(data, index) {
  idx_time <- time(dplyr::pull(data, !! index))
  out <- data %>% 
    select(- !! index)
  stats::ts(out, stats::start(idx_time), stats::frequency(idx_time))
}

#' @importFrom stats time
#' @importFrom stats tsp<- time
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

guess_frequency <- function(x) {
  UseMethod("guess_frequency")
}

guess_frequency.yearmonth <- function(x) {
  12 / pull_interval(x)$month
}

guess_frequency.yearquarter <- function(x) {
  4 / pull_interval(x)$quarter
}

guess_frequency.Date <- function(x) {
  7 / pull_interval(x)$day
}

guess_frequency.POSIXt <- function(x) {
  int <- pull_interval(x)
  number <- int$hour + int$minute / 60 + int$second / 3600
  if (number > 1 / 60) {
    return(24 / number)
  } else if (number > 1 / 3600 && number <= 1 / 60) {
    return(3600 * number)
  } else {
    return(3600 * 60 * number)
  }
}

