#' Return key variables
#'
#' `key()` returns a list of symbols; `key_vars()` gives a character vector.
#'
#' @param x A tsibble.
#'
#' @rdname key
#' @examples
#' key(pedestrian)
#' key_vars(pedestrian)
#'
#' key(tourism)
#' key_vars(tourism)
#' @export
key <- function(x) {
  UseMethod("key")
}

#' @export
key.default <- function(x) {
  abort(sprintf("Can't find the attribute key in class %s", class(x)[1]))
}

#' @export
key.tbl_ts <- function(x) {
  syms(key_vars(x))
}

#' @rdname key
#' @export
key_vars <- function(x) {
  UseMethod("key_vars")
}

#' @export
key_vars.tbl_ts <- function(x) {
  keys <- key_data(x)
  head(names(keys), -1L)
}

#' Keyed data
#'
#' @param .data A tsibble
#' @rdname key-data
#' @export
#' @examples
#' key_data(pedestrian)
key_data <- function(.data) {
  UseMethod("key_data")
}

#' @export
key_data.tbl_ts <- function(.data) {
  .data %@% key
}

#' @rdname key-data
#' @export
key_rows <- function(.data) {
  key_data(.data)[[".rows"]]
}

#' @rdname key-data
#' @usage NULL
#' @keywords internal
#' @export
n_keys <- function(x) {
  vec_size(key_data(x))
}

#' Default value for .drop argument for key
#'
#' @param .tbl A data frame
#' @keywords internal
#' @export
key_drop_default <- function(.tbl) {
  UseMethod("key_drop_default")
}

#' @export
key_drop_default.default <- function(.tbl) {
  TRUE
}

#' @export
key_drop_default.tbl_ts <- function(.tbl) {
  tryCatch({
    !identical(attr(key_data(.tbl), ".drop"), FALSE)
  }, error = function(e) {
    TRUE
  })
}

remove_key <- function(.data, .vars) {
  sel_key <- c(.vars[.vars %in% key_vars(.data)], ".rows")
  attr(.data, "key") <- key_data(.data)[sel_key]
  .data
}

is_key_dropped <- function(x) {
  if (!is_grouped_ts(x)) {
    key_drop_default(x)
  } else {
    key_vars <- key_vars(x)
    grp_vars <- group_vars(x)
    group_by_drop_default(x) && any(is.element(key_vars, grp_vars))
  }
}

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
  interval(x) %@% ".regular"
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
#' @export
is_grouped_ts <- function(x) {
  inherits(x, "grouped_ts")
}
