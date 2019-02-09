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
#' key_rows(pedestrian)
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

validate_key <- function(.data, .vars) {
  syms(unname(tidyselect::vars_select(names(.data), !!! .vars)))
}

remove_key <- function(.data, .vars) {
  sel_key <- c(.vars[.vars %in% key_vars(.data)], ".rows")
  attr(.data, "key") <- key_data(.data)[sel_key]
  .data
}

rename_key <- function(.data, .vars) {
  if (is_empty(.vars)) return(.data)

  old_key_vars <- key_vars(.data)
  if (is_empty(old_key_vars)) {
    names(attr(.data, "key")) <- ".rows"
    return(.data)
  }
  names <- names(.vars)
  idx <- .vars %in% old_key_vars
  names(.data)[idx] <- new_key_vars <- names[idx]
  names(attr(.data, "key")) <- c(new_key_vars, ".rows")
  .data
}

rename_group <- function(.data, .vars) {
  names <- names(.vars)
  old_grp_chr <- group_vars(.data)
  if (is_empty(old_grp_chr)) return(.data)

  idx <- .vars %in% old_grp_chr
  names(.data)[idx] <- new_grp_chr <- names[idx]
  names(attr(.data, "groups")) <- c(new_grp_chr, ".rows")
  .data
}
