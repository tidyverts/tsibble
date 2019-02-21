#' Deprecated functions
#'
#' @param x Other objects.
#' @rdname deprecated
#' @export
#' @keywords internal
as.tsibble <- function(x, ...) {
  as_tsibble(x, ...)
}

#' @rdname deprecated
#' @export
#' @keywords internal
#' @include gaps.R
fill_na <- function(.data, ..., .full = FALSE) {
  .Deprecated("fill_gaps()")
  fill_gaps(.data, ..., .full = .full)
}

#' @description
#' Find which row has duplicated key and index elements
#'
#' @rdname deprecated
#' @param data A `tbl_ts` object.
#' @param key Structural variable(s) that define unique time indices, used with
#' the helper [id]. If a univariate time series (without an explicit key),
#' simply call `id()`.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param fromLast `TRUE` does the duplication check from the last of identical
#' elements.
#'
#' @return A logical vector of the same length as the row number of `data`
#' @export
find_duplicates <- function(data, key = id(), index, fromLast = FALSE) {
  .Defunct("duplicates()")
}
