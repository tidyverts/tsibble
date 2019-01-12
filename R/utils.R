fast_nrow <- function(x) {
  .row_names_info(x, 2L)
}

split_period <- function(x) {
  output <- lubridate::seconds_to_period(x)
  list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  )
}

first_arg <- function(x) {
  purrr::compact(map(x, ~ dplyr::first(call_args(.x))))
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

min0 <- function(...) {
  min(..., na.rm = TRUE)
}

max0 <- function(...) {
  max(..., na.rm = TRUE)
}

is_even <- function(x) {
  (abs(x) %% 2) == 0
}

list_is_named <- function(x) {
  nms <- names(x)
  map_lgl(nms, ~ . != "")
}

has_tz <- function(x) {
  tz <- attr(x, "tzone")[[1]]
  if (is_null(tz) && !inherits(x, "POSIXct")) {
    FALSE
  } else {
    TRUE
  }
}
