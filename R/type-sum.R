#' @export
type_sum.yearmth <- function(x, ...) {
  "yrmth"
}

#' @export
type_sum.yearqtr <- function(x, ...) {
  "yrqtr"
}

#' @export
tbl_sum.tbl_ts <- function(x) {
  int_x <- interval(x)
  if (is_regular(x)) {
    first <- c(
      "A tsibble" = paste(dim_tbl_ts(x), "with", format(int_x), "interval")
    )
  } else {
    first <- c("A tsibble" = paste(dim_tbl_ts(x), format(int_x)))
  }
  c(first, "Keys" = paste(key_vars(x), collapse = ", "))
}

#' @export
tbl_sum.grouped_ts <- function(x) {
  info_tbl_ts <- tbl_sum.tbl_ts(x)
  c(info_tbl_ts,
    "Groups" = paste(group_vars(x), collapse = ", ")
  )
}
