#' Return key variables
#'
#' `key()` returns a list of symbols; `key_vars()` gives a character vector.
#'
#' @param x A data frame.
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
  attr(x, "key")
}

#' @rdname key
#' @export
key_vars <- function(x) {
  UseMethod("key_vars")
}

#' @export
key_vars.tbl_ts <- function(x) {
  map_chr(key(x), as_string)
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
  if (validate) {
    .data <- retain_tsibble(.data, key, index(.data))
  }
  build_tsibble_meta(
    .data, key = key, index = !! index(.data), index2 = !! index2(.data),
    regular = is_regular(.data), ordered = is_ordered(.data), 
    interval = interval(.data)
  )
}

#' Compute sizes of key variables
#'
#' @param x A data frame.
#'
#' @keywords internal
#' @rdname key-size
#' @export
#' @examples
#' key_size(pedestrian)
#' n_keys(pedestrian)
key_size <- function(x) {
  UseMethod("key_size")
}

#' @export
key_size.tbl_ts <- function(x) {
  idx <- key_indices(x)
  out <- as.integer(rowsum.default(rep_len(1L, length(idx)), idx, reorder = FALSE))
  if (has_length(out, 1)) {
    NROW(x)
  } else {
    out
  }
}

#' @rdname key-size
#' @keywords internal
#' @export
n_keys <- function(x) {
  UseMethod("n_keys")
}

#' @export
n_keys.tbl_ts <- function(x) {
  key <- key_vars(x)
  if (is_empty(key)) {
    1L
  } else {
    NROW(distinct(ungroup(as_tibble(x)), !!! syms(key)))
  }
}

#' @rdname key-size
#' @keywords internal
#' @export
key_indices <- function(x) {
  UseMethod("key_indices")
}

#' @export
key_indices.tbl_ts <- function(x) {
  key_vars <- key_vars(x)
  grped_key <- grouped_df(x, key_vars)
  group_indices(grped_key)
}

validate_key <- function(.data, .vars) {
  syms(tidyselect::vars_select(names(.data), !!! .vars))
}

remove_key <- function(.data, .vars) {
  attr(.data, "key") <- syms(.vars[.vars %in% key_vars(.data)])
  .data
}

rename_key <- function(.data, .vars) {
  if (is_empty(.vars)) return(.data)

  old_key_vars <- key_vars(.data)
  if (is_empty(old_key_vars)) {
    attr(.data, "key") <- id()
    return(.data)
  }
  names <- names(.vars)
  idx <- .vars %in% old_key_vars
  names(.data)[idx] <- new_key_vars <- names[idx]
  attr(.data, "key") <- syms(new_key_vars)
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
