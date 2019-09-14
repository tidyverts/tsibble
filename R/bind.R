#' @method rbind tbl_ts
#' @export
rbind.tbl_ts <- function(...) {
  data <- list2(...)[[1]]
  x <- vec_rbind(...)
  x <- retain_tsibble(x, key(data), index(data))
  int_int <- map_lgl(list2(...), ~ (. %@% "interval") %@% ".regular")
  int <- reduce(int_int, `||`)
  update_meta(x, data, ordered = NULL, interval = int)
}

#' @method cbind tbl_ts
#' @export
cbind.tbl_ts <- function(...) {
  bind_cols(...)
}
