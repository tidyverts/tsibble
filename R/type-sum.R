#' @export
type_sum.yearmon <- function(x, ...) {
  "yrmon"
}

#' @export
type_sum.yearqtr <- function(x, ...) {
  "yrqtr"
}

#' @export
tbl_sum.tbl_ts <- function(x) {
  key_var <- key(x)
  int_x <- interval(x)
  if (is_regular(x)) {
    first <- c(
      "A tsibble" = paste(dim_tbl_ts(x), "with", format(int_x), "interval")
    )
  } else {
    first <- c("A tsibble" = paste(dim_tbl_ts(x), format(int_x)))
  }
  c(first, "Keys" = paste(format(key_var), collapse = ", "))
}
