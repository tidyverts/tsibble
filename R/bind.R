#' @method rbind tbl_ts
#' @export
rbind.tbl_ts <- function(...) {
  data <- list2(...)[[1]]
  x <- dplyr::bind_rows(...)
  x <- retain_tsibble(x, key(data), index(data))
  build_tsibble_meta(
    x, key = key(data), index = !! index(data),
    index2 = !! index2(data), regular = is_regular(data)
  )
}

#' @method cbind tbl_ts
#' @export
cbind.tbl_ts <- function(...) {
  data <- list2(...)[[1]]
  x <- dplyr::bind_cols(...)
  build_tsibble_meta(
    x, key = key(data), index = !! index(data),
    index2 = !! index2(data), regular = is_regular(data),
    ordered = is_ordered(data), interval = interval(data)
  )
}
