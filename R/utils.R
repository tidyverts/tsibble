split_period <- function(x) {
  output <- lubridate::seconds_to_period(x)
  list(
    year = output$year, month = output$month, day = output$day,
    hour = output$hour, minute = output$minute, second = output$second
  )
}

# regular time interval is obtained from the greatest common divisor of positive
# time distances.
gcd_interval <- function(x) {
  if (has_length(x, 1)) { # only one time index
    NA_real_
  } else if (is_integerish(x)) {
    gcd_vector(x)
  } else {
    gcd_vector_r(unique(round(abs(diff(x)), digits = 6)))
  }
}

gcd2 <- function(a, b) {
  if (isTRUE(all.equal(b, 0))) a else gcd2(b, a %% b)
}

gcd_vector_r <- function(x) Reduce(gcd2, x)

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
