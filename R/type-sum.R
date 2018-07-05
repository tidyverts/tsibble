#' @export
type_sum.tbl_ts <- function(x) {
  "tsibble"
}

#' @export
type_sum.yearweek <- function(x) {
  "week"
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
  fnt_int <- format(int_x)
  first <- c("A tsibble" = paste(dim_tbl_ts(x), surround(fnt_int, "[")))
  if (is_empty(key(x))) {
    return(first)
  }
  c(first, key_sum(x))
}

#' @export
tbl_sum.grouped_ts <- function(x) {
  n_grps <- big_mark(n_groups(x))
  if (has_length(n_grps, 0)) {
    n_grps <- "?"
  }
  grps <- group_vars(x)
  idx2 <- quo_name(index2(x))
  grp_var <- setdiff(grps, idx2)
  idx_suffix <- paste("@", idx2)
  if (is_empty(grp_var)) {
    return(c(
      NextMethod(), 
      "Groups" = paste(idx_suffix, surround(n_grps, "["))
    ))
  } else if (has_length(grps, length(grp_var))) {
    return(c(
      NextMethod(), 
      "Groups" = paste(paste_comma(grp_var), surround(n_grps, "["))
    ))
  } else {
    return(c(
      NextMethod(), 
      "Groups" = paste(paste_comma(grp_var), idx_suffix, surround(n_grps, "["))
    ))
  }
}

#' Summary of key variables
#'
#' @param x An object that contains "key".
#'
#' @export
#' @examples
#' key_sum(pedestrian)
key_sum <- function(x) {
  UseMethod("key_sum")
}

#' @export
key_sum.default <- function(x) {
  n_keys <- big_mark(n_keys(x))
  c("Key" = paste(paste_comma(format(key(x))), surround(n_keys, "[")))
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
#' @return `TRUE`/`FALSE` or `NA` (unsure)
#' @export
#' @examples
#' index_valid(seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1))
index_valid <- function(x) {
  UseMethod("index_valid")
}

#' @export
index_valid.POSIXct <- function(x) { 
  TRUE 
}

#' @export
index_valid.difftime <- index_valid.POSIXct

#' @export
index_valid.Date <- index_valid.POSIXct

#' @export
index_valid.yearweek <- index_valid.POSIXct

#' @export
index_valid.yearmonth <- index_valid.POSIXct

#' @export
index_valid.yearmth <- index_valid.yearmonth

#' @export
index_valid.yearquarter <- index_valid.POSIXct

#' @export
index_valid.yearqtr <- index_valid.yearquarter

#' @export
index_valid.numeric <- function(x) {
  NA
}

#' @export
index_valid.integer <- index_valid.numeric

#' @export
index_valid.default <- function(x) {
  FALSE
}
