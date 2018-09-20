split_period <- function(x) {
  output <- lubridate::seconds_to_period(x)
  list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  )
}

first_arg <- function(x) {
  lst_env <- purrr::map(x, get_env)
  purrr::compact(
    purrr::map2(x, lst_env, ~ new_quosure(dplyr::first(call_args(.x)), .y))
  )
}

# regular time interval is obtained from the greatest common divisor of positive
# time distances.
gcd_interval <- function(x) {
  if (has_length(x, 1)) { # only one time index
    NA_real_
  } else {
    gcd_vector(x)
  }
}

validate_vars <- function(j, x) { # j = quos/chr/dbl
  tidyselect::vars_select(.vars = x, !!! j)
}

# this function usually follows validate_vars()
has_index <- function(j, x) {
  is_index_null(x)
  index <- c(quo_name(index(x)), quo_name(index2(x)))
  any(index %in% j)
}

has_distinct_key <- function(j, x) {
  key_vars <- key_flatten(key_distinct(key(x)))
  all(key_vars %in% j)
}

has_any_key <- function(j, x) {
  key_vars <- key_flatten(key(x))
  any(key_vars %in% j)
}

min0 <- function(...) {
  min(..., na.rm = TRUE)
}

max0 <- function(...) {
  max(..., na.rm = TRUE)
}

is_even <- function(x) {
  (abs(x) %% 2) == 0
}
