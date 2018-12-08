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
fill_na <- fill_gaps.tbl_ts

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
  .Deprecated("duplicates()")
  key <- use_id(data, !! enquo(key))
  index <- validate_index(data, enquo(index))

  grouped_df(data, vars = key) %>%
    mutate(!! "zzz" := duplicated.default(!! index, fromLast = fromLast)) %>%
    dplyr::pull(!! "zzz")
}
