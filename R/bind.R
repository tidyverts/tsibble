#' @method rbind tbl_ts
#' @export
rbind.tbl_ts <- function(...) {
  data <- list2(...)[[1]]
  x <- dplyr::bind_rows(...)
  x <- retain_tsibble(x, key(data), index(data))
  update_tsibble(x, data, ordered = NULL)
}

#' @method cbind tbl_ts
#' @export
cbind.tbl_ts <- function(...) {
  dplyr::bind_cols(...)
}
