#' Add custom index support for a tsibble
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' S3 method to add an index type support for a tsibble.
#'
#' @param x An object of index type supported by tsibble.
#'
#' @details This method is primarily used for adding an index type support in
#' [as_tsibble].
#' @seealso [interval_pull] for obtaining interval for regularly spaced time.
#' @return `TRUE`/`FALSE` or `NA` (unsure)
#' @export
#' @examples
#' index_valid(seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1))
index_valid <- function(x) {
  UseMethod("index_valid")
}

#' @export
index_valid.POSIXt <- function(x) {
  TRUE
}

#' @export
index_valid.difftime <- index_valid.POSIXt

#' @export
index_valid.Date <- index_valid.POSIXt

#' @export
index_valid.yearweek <- index_valid.POSIXt

#' @export
index_valid.yearmonth <- index_valid.POSIXt

#' @export
index_valid.yearmon <- function(x) {
  abort(c("No index support for class \"yearmon\".",
    i = "Please use `yearmonth()` to convert first."))
}

#' @export
index_valid.yearquarter <- index_valid.POSIXt

#' @export
index_valid.yearqtr <- function(x) {
  abort(c("No index support for class \"yearqtr\".",
    i = "Please use `yearquarter()` to convert first."))
}

#' @export
index_valid.nanotime <- index_valid.POSIXt

#' @export
index_valid.ordered <- index_valid.POSIXt

#' @export
index_valid.Period <- index_valid.POSIXt

#' @export
index_valid.numeric <- function(x) {
  NA
}

#' @export
index_valid.integer <- index_valid.numeric

#' @export
index_valid.default <- function(x) {
  FALSE
}
