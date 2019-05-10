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

#' Keying data
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
  NROW(key_data(x))
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
