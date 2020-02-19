#' Deprecated functions
#'
#' @description
#' * `is.tsibble()` \lifecycle{defunct}
#' * `time_unit()` \lifecycle{deprecated}
#' * `units_since()` \lifecycle{deprecated}
#'
#' @param x Other objects.
#' @param .data A tsibble.
#' @param ... Variables passed to tsibble()/as_tsibble().
#'
#' @rdname deprecated
#' @export
#' @keywords internal
is.tsibble <- function(x) {
  lifecycle::deprecate_stop("0.8.4", "is.tsibble()", "is_tsibble()")
  is_tsibble(x)
}

#' @rdname deprecated
#' @export
#' @keywords internal
time_unit <- function(x) {
  lifecycle::deprecate_warn("0.9.0", "time_unit()", "default_time_units()")
  default_time_units(x)
}

#' @export
#' @rdname deprecated
#' @keywords internal
units_since <- function(x) {
  lifecycle::deprecate_warn("0.9.0", "units_since()", "as.double()")
  UseMethod("units_since")
}

#' @export
units_since.numeric <- function(x) {
  if (min0(x) > 1581 && max0(x) < 2500) { # Input is years
    x - 1970L
  } else {
    x
  }
}

#' @export
units_since.yearweek <- function(x) {
  as.numeric((as_date(x) - as_date("1969-12-29")) / 7)
}

#' @export
units_since.yearmonth <- function(x) {
  as.numeric((year(x) - 1970) * 12 + month(x) - 1)
}

#' @export
units_since.yearquarter <- function(x) {
  as.numeric((year(x) - 1970) * 4 + quarter(x) - 1)
}

#' @export
units_since.Date <- function(x) {
  as.numeric(x)
}

#' @export
units_since.POSIXct <- function(x) {
  as.numeric(x)
}
