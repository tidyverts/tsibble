#' @export
type_sum.yearmth <- function(x) {
  "mth"
}

#' @export
type_sum.yearqtr <- function(x) {
  "qtr"
}

#' @export
tbl_sum.tbl_ts <- function(x) {
  int_x <- interval(x)
  first <- c("A tsibble" = paste(dim_tbl_ts(x), surround(format(int_x), "[")))
  if (is_empty(key(x))) {
    return(first)
  } else {
    c(
      first,
      "Keys" = paste(key_vars(x), collapse = ", ")
    )
  }
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
index_sum <- function(x) {
  UseMethod("index_sum")
}

#' @export
index_sum.POSIXct <- function(x) {
  "dttm"
}

#' @export
index_sum.difftime <- function(x) {
  "time"
}

#' @export
index_sum.Date <- function(x) {
  "date"
}

#' @export
index_sum.yearmth <- function(x) {
  "yrmth"
}

#' @export
index_sum.yearqtr <- function(x) {
  "yrqtr"
}

#' @export
index_sum.numeric <- function(x) {
  "dbl"
}

#' @export
index_sum.integer <- function(x) {
  "int"
}

#' @export
index_sum.default <- function(x) {
  NA_character_
}
