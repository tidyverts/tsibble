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
