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

#' @export
`[[.key` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
}

#' @export
`[.key` <- function(x, i, j, drop = FALSE) {
  NextMethod()
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

#' @rdname key
#' @export
#' @examples
#' # unkey() only works for a tsibble with 1 key size ----
#' sx <- pedestrian %>%
#'   filter(Sensor == "Southern Cross Station")
#' unkey(sx)
unkey <- function(x) {
  UseMethod("unkey")
}

#' @export
unkey.tbl_ts <- function(x) {
  nkey <- n_keys(x)
  if (nkey < 2 || nkey == NROW(x)) {
    attr(x, "key") <- id()
    x
  } else {
    abort("Can't apply to a tsibble of more than 1 key size.")
  }
}

#' Compute sizes of key variables
#'
#' @param x A data frame.
#'
#' @examples
#' key_size(pedestrian)
#' n_keys(pedestrian)
#'
#' @rdname key-size
#' @export
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
  syms(unname(tidyselect::vars_select(names(.data), !!! .vars)))
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

rename_group <- function(.data, .vars) {
  names <- names(.vars)
  old_grp_chr <- group_vars(.data)
  idx <- .vars %in% old_grp_chr
  names(.data)[idx] <- new_grp_chr <- names[idx]
  group_by(.data, !!! syms(new_grp_chr))
}
