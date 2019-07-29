#' New tsibble data and append new observations to a tsibble
#'
#' @description
#' \Sexpr[results=rd, stage=render]{tsibble:::lifecycle("stable")}
#'
#' @param .data A `tbl_ts`.
#' @param n An integer indicates the number of key-index pair to append.
#' @param ... Passed to individual S3 method.
#'
#' @rdname new-data
#' @export
new_data <- function(.data, n = 1L, ...) {
  if (!is_integerish(n, 1) && any(n > 0)) {
    abort("Argument `n` must be a positive integer.")
  }

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
new_data.tbl_ts <- function(.data, n = 1L, keep_all = FALSE, ...) {
  not_regular(.data)
  abort_unknown_interval(int <- interval(.data))

  idx <- index(.data)
  tunit <- time_unit(int)

  grped_df <- new_grouped_df(.data, groups = key_data(.data))
  last_entry <- summarise(grped_df, !!idx := max(!!idx))

  nc <- NCOL(last_entry)
  new_lst <- new_list(NROW(last_entry))
  for (i in seq_len(NROW(last_entry))) {
    lst_i <- new_lst[[i]] <- as.list(last_entry[i, ])
    new_lst[[i]][[nc]] <- seq(lst_i[[nc]], by = tunit, length.out = n + 1)[-1]
    new_lst[[i]] <- as_tibble(new_lst[[i]])
  }
  out <- bind_rows(!!!new_lst)
  if (keep_all) {
    out <- bind_rows(.data[0L, ], out)
  } else { # reorder column names according to the data input
    cn <- setdiff(names(.data), measured_vars(.data))
    out <- select(out, !!!cn)
  }
  update_meta(out, .data, ordered = TRUE, interval = interval(.data))
}

#' @description
#' \Sexpr[results=rd, stage=render]{tsibble:::lifecycle("maturing")}
#'
#' `append_row()`: add new rows to the end of a tsibble by filling a key-index
#' pair and `NA` for measured variables.
#'
#' `append_case()` is an alias of `append_row()`.
#' @rdname new-data
#' @export
#' @examples
#' tsbl <- tsibble(
#'   date = rep(as.Date("2017-01-01") + 0:2, each = 2),
#'   group = rep(letters[1:2], 3),
#'   value = rnorm(6),
#'   key = group
#' )
#' append_row(tsbl)
#' append_row(tsbl, n = 2)
append_row <- function(.data, n = 1L, ...) {
  UseMethod("append_row")
}

#' @export
append_row.tbl_ts <- function(.data, n = 1L, ...) {
  new_data <- new_data(.data, n = n)
  out <- bind_rows(.data, new_data)
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
