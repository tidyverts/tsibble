
#' Return measured variables
#'
#' @param x A `tbl_ts`.
#'
#' @rdname measured-vars
#' @examples
#' measures(pedestrian)
#' measures(tourism)
#'
#' measured_vars(pedestrian)
#' measured_vars(tourism)
#' @export
measures <- function(x) {
  UseMethod("measures")
}

#' @export
measures.tbl_ts <- function(x) {
  syms(measured_vars(x))
}

#' @rdname measured-vars
#' @export
measured_vars <- function(x) {
  UseMethod("measured_vars")
}

#' @export
measured_vars.tbl_ts <- function(x) {
  all_vars <- names(x)
  key_vars <- key_vars(x)
  idx_var <- index_var(x)
  setdiff(all_vars, c(key_vars, idx_var))
}

#' Return index variable from a tsibble
#'
#' @param x A tsibble object.
#' @rdname index-rd
#' @examples
#' index(pedestrian)
#' index_var(pedestrian)
#' @export
index <- function(x) {
  sym(index_var(x))
}

#' @rdname index-rd
#' @export
index_var <- function(x) {
  not_tsibble(x)
  as_string(x %@% "index")
}

#' @rdname index-rd
#' @export
index2 <- function(x) {
  sym(index2_var(x))
}

#' @rdname index-rd
#' @export
index2_var <- function(x) {
  not_tsibble(x)
  x %@% "index2"
}

#' Meta-information of a tsibble
#'
#' * `interval()` returns an interval of a tsibble.
#' * `is_regular` checks if a tsibble is spaced at regular time or not.
#' * `is_ordered` checks if a tsibble is ordered by key and index.
#'
#' @param x A tsibble object.
#' @rdname regular
#' @examples
#' interval(pedestrian)
#' @export
interval <- function(x) {
  not_tsibble(x)
  x %@% "interval"
}

#' @rdname regular
#' @examples
#' is_regular(pedestrian)
#' is_ordered(pedestrian)
#' @export
is_regular <- function(x) {
  !is_empty(interval(x))
}

#' @rdname regular
#' @export
is_ordered <- function(x) {
  not_tsibble(x)
  attr(x %@% "index", "ordered")
}

#' If the object is a tsibble
#'
#' @description
#' \lifecycle{stable}
#'
#' @param x An object.
#'
#' @return TRUE if the object inherits from the tbl_ts class.
#' @rdname is-tsibble
#' @aliases is.tsibble
#' @examples
#' # A tibble is not a tsibble ----
#' tbl <- tibble(
#'   date = seq(as.Date("2017-10-01"), as.Date("2017-10-31"), by = 1),
#'   value = rnorm(31)
#' )
#' is_tsibble(tbl)
#'
#' # A tsibble ----
#' tsbl <- as_tsibble(tbl, index = date)
#' is_tsibble(tsbl)
#' @export
is_tsibble <- function(x) {
  inherits(x, "tbl_ts")
}

#' @rdname is-tsibble
#' @usage NULL
#' @export
is.tsibble <- is_tsibble

#' @rdname is-tsibble
#' @export
is_grouped_ts <- function(x) {
  inherits(x, "grouped_ts")
}

#' @rdname is-tsibble
#' @usage NULL
#' @export
is.grouped_ts <- is_grouped_ts
