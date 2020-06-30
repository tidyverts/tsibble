#' New tsibble data and append new observations to a tsibble
#'
#' \lifecycle{stable}
#'
#' @param .data A `tbl_ts`.
#' @param n An integer indicates the number of key-index pair to append. If
#' * `n > 0`, future observations
#' * `n < 0`, past observations
#' @param ... Passed to individual S3 method.
#'
#' @rdname new-data
#' @export
new_data <- function(.data, n = 1L, ...) {
  UseMethod("new_data")
}

#' @param keep_all If `TRUE` keep all the measured variables as well as index
#' and key, otherwise only index and key.
#' @rdname new-data
#' @export
#' @examples
#' new_data(pedestrian)
#' new_data(pedestrian, keep_all = TRUE)
#' new_data(pedestrian, n = 3)
#' new_data(pedestrian, n = -2)
new_data.tbl_ts <- function(.data, n = 1L, keep_all = FALSE, ...) {
  if (!is_integerish(n, 1)) {
    abort("Argument `n` must be an integer.")
  }
  abort_if_irregular(.data)
  abort_unknown_interval(int <- interval(.data))

  idx <- index(.data)
  tunit <- default_time_units(int)

  key_data <- key_data(.data)
  grped_df <- new_grouped_df(.data, groups = key_data)
  if (n >= 0) {
    is_ord <- TRUE
    last_entry <- summarise(grped_df, !!idx := max(!!idx))
  } else {
    is_ord <- NULL
    last_entry <- summarise(grped_df, !!idx := min(!!idx))
    tunit <- -tunit
  }
  if (length(key_data) == 1) { # no key
    regrped_df <- last_entry
  } else {
    meta_grps <- mutate(key_data, .rows = list2(!!!vec_seq_along(last_entry)))
    regrped_df <- new_grouped_df(last_entry, groups = meta_grps)
  }
  new_lst <- mutate(regrped_df, 
    !!idx := list2(
      !!idx := seq_generator(!!idx, tunit, length_out = abs(n) + 1)[-1]))

  out <- unwrap(ungroup(new_lst), .col = !!idx)
  if (keep_all) {
    out <- vec_rbind(grped_df[0L, ], out)
  } else { # reorder column names according to the data input
    out <- out[setdiff(names(.data), measured_vars(.data))]
  }
  update_meta(out, .data, ordered = is_ord, interval = interval(.data))
}

#' @export
new_data.grouped_ts <- function(.data, n = 1L, keep_all = FALSE, ...) {
  inform(c(
    "Grouping structure is ignored.",
    i = "`ungroup()` to silence this message."
  ))
  NextMethod()
}

#' @description
#' `append_row()`: add new rows to the start/end of a tsibble by filling a key-index
#' pair and `NA` for measured variables.
#'
#' `append_case()` is an alias of `append_row()`.
#' @rdname new-data
#' @export
#' @examples
#'
#' tsbl <- tsibble(
#'   date = rep(as.Date("2017-01-01") + 0:2, each = 2),
#'   group = rep(letters[1:2], 3),
#'   value = rnorm(6),
#'   key = group
#' )
#' append_row(tsbl)
#' append_row(tsbl, n = 2)
#' append_row(tsbl, n = -2)
append_row <- function(.data, n = 1L, ...) {
  UseMethod("append_row")
}

#' @export
append_row.tbl_ts <- function(.data, n = 1L, ...) {
  new_data <- new_data(.data, n = n)
  out <- vec_rbind(as_tibble(.data), as_tibble(new_data))
  ord <- is_ordered(.data)
  if (ord) ord <- NULL # re-order
  update_meta(out, .data, ordered = ord, interval = interval(.data))
}

#' @export
append_row.data.frame <- function(.data, n = 1L, ...) {
  abort("Do you need `tibble::add_row()` for a `tbl_df`/`data.frame`?")
}

#' @rdname new-data
#' @export
#' @usage NULL
append_case <- append_row
