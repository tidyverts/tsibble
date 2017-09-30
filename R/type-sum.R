#' @export
type_sum.yearmth <- function(x) {
  "yrmth"
}

#' @export
type_sum.yearqtr <- function(x) {
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


#' Extensible index type to tsibble
#'
#' @param x An object to be added to index types that tsibble supports.
#'
#' @export
#'
idx_sum <- function(x) {
  UseMethod("idx_sum")
}

#' @export
idx_sum.POSIXct <- function(x) {
  "dttm"
}

#' @export
idx_sum.difftime <- function(x) {
  "time"
}

#' @export
idx_sum.Date <- function(x) {
  "date"
}

#' @export
idx_sum.yearmth <- function(x) {
  "yrmth"
}

#' @export
idx_sum.yearqtr <- function(x) {
  "yrqtr"
}

#' @export
idx_sum.numeric <- function(x) {
  "dbl"
}

#' @export
idx_sum.integer <- function(x) {
  "int"
}

#' @export
idx_sum.default <- function(x) {
  NA_character_
}
