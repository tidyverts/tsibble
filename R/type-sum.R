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
  if (is_regular(x)) {
    int_x <- interval(x)
    first <- c("A tsibble" = paste(dim_tbl_ts(x), "with", format(int_x), "interval"))
  } else {
    first <- c("A tsibble" = paste(dim_tbl_ts(x), "[!]"))
    
  }
  c(first, "Keys" = format(key_var))
}
