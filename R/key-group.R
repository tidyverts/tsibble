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
  attr(.data, "key")
}

#' @rdname key-data
#' @export
key_rows <- function(.data) {
  key_data(.data)[[".rows"]]
}

#' Change key variables for a given `tbl_ts`
#'
#' @param .data A `tbl_ts`.
#' @param ... Variables to construct the key.
#'
#' @export
#' @examples
#' # By removing `State` from key, it is a valid tsibble too.
#' tourism %>%
#'   key_by(Region, Purpose)
key_by <- function(.data, ...) {
  UseMethod("key_by")
}

#' @export
key_by.tbl_ts <- function(.data, ...) {
  exprs <- enexprs(...)
  key <- validate_key(.data, exprs)
  validate <- !all(is.element(key(.data), key))
  idx <- index(.data)
  if (validate) {
    .data <- retain_tsibble(.data, key, idx)
  }
  build_tsibble(
    .data, key = key, index = !! idx, index2 = !! index2(.data),
    regular = is_regular(.data), ordered = is_ordered(.data), 
    interval = interval(.data), validate = FALSE
  )
}

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

#' @export
groups.tbl_ts <- function(x) {
  NULL
}

#' @export
groups.grouped_ts <- function(x) {
  res <- as_grouped_df(x)
  groups(res)
}

#' @export
group_vars.tbl_ts <- function(x) {
  character(0L)
}

#' @export
group_data.grouped_ts <- function(.data) {
  res <- as_grouped_df(.data)
  group_data(res)
}

#' @export
group_rows.grouped_ts <- function(.data) {
  res <- as_grouped_df(.data)
  group_rows(res)
}

#' @export
group_vars.grouped_ts <- function(x) {
  res <- as_grouped_df(x)
  group_vars(res)
}

#' @export
group_size.grouped_ts <- function(x) {
  res <- as_grouped_df(x)
  group_size(res)
}

#' @export
n_groups.tbl_ts <- function(x) {
  res <- as_grouped_df(x)
  n_groups(res)
}

#' @export
group_indices.grouped_ts <- function(.data, ...) {
  res <- as_grouped_df(.data)
  group_indices(res)
}

rename_group <- function(.data, .vars) {
  names <- names(.vars)
  old_grp_chr <- group_vars(.data)
  idx <- .vars %in% old_grp_chr
  names(.data)[idx] <- new_grp_chr <- names[idx]
  group_by(.data, !!! syms(new_grp_chr))
}
