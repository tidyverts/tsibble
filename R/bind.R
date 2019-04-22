#' @method rbind tbl_ts
#' @export
rbind.tbl_ts <- function(...) {
  data <- list2(...)[[1]]
  x <- dplyr::bind_rows(...)
  x <- retain_tsibble(x, key(data), index(data))
  int_int <- map_lgl(list2(...), ~ !is_empty(. %@% "interval"))
  int <- purrr::reduce(int_int, `||`)
  update_meta(x, data, ordered = NULL, interval = int)
}

#' @method cbind tbl_ts
#' @export
cbind.tbl_ts <- function(...) {
  dplyr::bind_cols(...)
}
