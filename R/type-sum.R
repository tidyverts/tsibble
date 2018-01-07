#' @export
type_sum.tbl_ts <- function(x) {
  "tsibble"
}

#' @export
type_sum.yearmonth <- function(x) {
  "mth"
}

#' @export
type_sum.yearquarter <- function(x) {
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
      "Keys" = paste(paste_comma(key_vars(x)), surround(n_keys(x), "["))
    )
  }
}

#' @export
tbl_sum.grouped_ts <- function(x) {
  n_grps <- n_groups(x)
  c(NextMethod(),
    "Groups" = paste(paste_comma(group_vars(x)), surround(n_grps, "["))
  )
}


#' Extensible index type to tsibble
#'
#' S3 method to add an index type support for a tsibble.
#'
#' @param x An object of index type that the tsibble supports.
#'
#' @details This method is primarily used for adding an index type support in 
#' [as_tsibble].
#' @seealso [pull_interval] for obtaining interval for regularly spaced time.
#' @export
#' @examples
#' index_sum(seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1))
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
index_sum.yearmonth <- function(x) {
  "mth"
}

#' @export
index_sum.yearmth <- index_sum.yearmonth

#' @export
index_sum.yearquarter <- function(x) {
  "qtr"
}

#' @export
index_sum.yearqrt <- index_sum.yearquarter

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
