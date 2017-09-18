#' @export
type_sum.yearmon <- function(x, ...) {
  "yrmon"
}

#' @export
type_sum.yearqtr <- function(x, ...) {
  "yrqtr"
}

tbl_sum.tbl_ts <- function(x) {
  int_x <- interval(x)
  key_var <- key(x)
  c(
    "A tsibble" = paste(dim_tbl_ts(x), "with", format(int_x), "interval"),
    "Keys" = format(key_var)
  )
}
