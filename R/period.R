# Unlike zoo::yearmon and zoo::yearqtr based on numerics,
# tsibble::yearmth and tsibble::yearqtr are based on the "Date" class.

#' Represent year-month or year-quarter objects
#'
#' @param x A vector of date-time, date.
#' @param tz Time zone associated with the `POSIXt` object x is the default.
#' @param ... Other arguments applied to an individual class.
#'
#' @return A year-month (`yearmth`) or year-quarter (`yearqtr`) object.
#' @rdname period
#' @export
#'
#' @examples
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = 30)
#' yearmth(x)
#' yearqtr(x)
#' year(x)
yearmth <- function(x, ...) {
  UseMethod("yearmth")
}

#' @rdname period
#' @export
yearmth.Date <- function(x, ...) {
  # convert all to the first day of the month
  result <- lubridate::floor_date(x, unit = "months")
  structure(result, class = c("yearmth", "Date"))
}

#' @rdname period
#' @export
yearmth.POSIXt <- function(x, tz = NULL, ...) {
  date <- lubridate::as_date(x, tz = tz)
  result <- lubridate::floor_date(date, unit = "months")
  structure(result, class = c("yearmth", "Date"))
}

#' @rdname period
#' @export
yearmth.yearmth <- function(x, ...) {
  structure(x, class = c("yearmth", "Date"))
}

#' @rdname period
#' @export
yearmth.numeric <- function(x, ...) {
  year <- trunc(x)
  month <- formatC(trunc((x %% 1) * 12 + 1), flag = 0, width = 2)
  result <- as.Date(paste(year, month, "01", sep = "-"))
  structure(result, class = c("yearmth", "Date"))
}

#' @export
format.yearmth <- function(x, format = "%Y %b", ...) {
  format.Date(x, format = format, ...)
}

#' @export
print.yearmth <- function(x, format = "%Y %b", ...) {
  print(format(x, format = format, ...))
  invisible(x)
}

#' @rdname period
#' @export
yearqtr <- function(x, ...) {
  UseMethod("yearqtr")
}

#' @rdname period
#' @export
yearqtr.POSIXt <- function(x, tz = NULL, ...) {
  date <- lubridate::as_date(x, tz = tz)
  result <- lubridate::floor_date(date, unit = "quarters")
  structure(result, class = c("yearqtr", "Date"))
}

#' @rdname period
#' @export
yearqtr.Date <- function(x, ...) {
  result <- lubridate::floor_date(x, unit = "quarters")
  structure(result, class = c("yearmth", "Date"))
}

#' @rdname period
#' @export
yearqtr.yearqtr <- function(x, ...) {
  structure(x, class = c("yearqtr", "Date"))
}

#' @rdname period
#' @export
yearqtr.numeric <- function(x, ...) {
  year <- trunc(x)
  quarter <- formatC(trunc((x %% 1) * 4 + 1), flag = 0, width = 2)
  result <- as.Date(paste(year, quarter, "01", sep = "-"))
  structure(result, class = c("yearqtr", "Date"))
}

#' @export
format.yearqtr <- function(x, format = "%Y Q%q", ...) {
  year <- lubridate::year(x)
  year_sym <- "%Y"
  if (grepl("%y", format)) {
    year <- sprintf("%02d", year %% 100)
    year_sym <- "%y"
  } else if (grepl("%C", format)) {
    year <- year %/% 100
    year_sym <- "%C"
  }
  qtr <- lubridate::quarter(x)
  qtr_sub <- purrr::map_chr(qtr, ~ gsub("%q", ., x = format))
  year_sub <- purrr::map2_chr(year, qtr_sub, ~ gsub(year_sym, .x, x = .y))
  year_sub
}

#' @export
print.yearqtr <- function(x, format = "%Y Q%q", ...) {
  print(format.yearqtr(x, format = format, ...))
  invisible(x)
}

#' @rdname period
#' @export
year <- function(x) {
  UseMethod("year")
}

#' @rdname period
#' @export
year.POSIXt <- function(x, tz = NULL, ...) {
  date <- lubridate::as_date(x, tz = tz)
  result <- lubridate::floor_date(date, unit = "years")
  structure(result, class = c("year", "Date"))
}

#' @rdname period
#' @export
year.Date <- function(x) {
  result <- lubridate::floor_date(x, unit = "years")
  structure(result, class = c("year", "Date"))
}

#' @rdname period
#' @export
year.year <- function(x) {
  structure(x, class = c("year", "Date"))
}

#' @rdname period
#' @export
year.numeric <- function(x) {
  result <- as.Date(paste(x, "01", "01", sep = "-"))
  structure(result, class = c("year", "Date"))
}

#' @rdname period
#' @export
year.integer <- year.numeric

#' @export
format.year <- function(x, format = "%Y", ...) {
  format.Date(x, format = format, ...)
}

#' @export
print.year <- function(x, format = "%Y", ...) {
  print(format(x, format = format, ...))
  invisible(x)
}
