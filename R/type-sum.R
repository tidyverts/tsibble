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

key_header <- function(x) {
  int_x <- interval(x)
  fnt_int <- format(int_x)
  first <- c("A tsibble" = paste(dim_tbl_ts(x), surround(fnt_int, "[")))
  if (is_empty(key(x))) {
    return(first)
  }
  n_keys <- big_mark(n_keys(x))
  c(
    first,
    "Keys" = paste(paste_comma(format(key(x))), surround(n_keys, "["))
  )
}

#' @export
tbl_sum.tbl_ts <- function(x) {
  result <- key_header(x)
  idx2 <- index2(x)
  if (!identical(index(x), idx2)) {
    idx_suffix <- paste("@", quo_text(idx2))
    result <- c(result, "Groups" = idx_suffix)
  }
  result
}

#' @export
tbl_sum.grouped_ts <- function(x) {
  result <- key_header(x)
  n_grps <- big_mark(n_groups(x))
  if (has_length(n_grps, 0)) {
    n_grps <- "?"
  }
  grps <- paste(paste_comma(group_vars(x)), surround(n_grps, "["))
  idx2 <- index2(x)
  if (identical(index(x), idx2)) {
    return(c(result, "Groups" = grps))
  }
  idx_suffix <- paste("@", quo_text(idx2))
  c(result, "Groups" = paste(grps, idx_suffix))
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
index_valid.yearqrt <- index_valid.yearquarter

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
