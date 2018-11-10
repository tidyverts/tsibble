#' New tsibble data
#'
#' @inheritParams append_row
#' @param keep_all If `TRUE` keep all the measured variables as well as index
#' and key, otherwise only index and key.
#'
#' @seealso [append_row] for appending new observations to a tsibble
#' @export
#' @examples
#' new_data(pedestrian)
#' new_data(pedestrian, keep_all = TRUE)
#' new_data(pedestrian, n = 3)
new_data <- function(.data, n = 1L, keep_all = FALSE) {
  if (!is_tsibble(.data)) {
    abort(sprintf("`.data` must be a tsibble, not `%s`.", class(.data)[1]))
  }
  not_regular(.data)
  unknown_interval(int <- interval(.data))

  if (!is_integerish(n, 1) && n > 0) {
    abort("Argument `n` must be a positive integer.")
  }

  idx <- index(.data)
  tunit <- time_unit(int)

  grped_df <- grped_df_by_key(.data)
  last_entry <- summarise(grped_df, !! idx := max(!! idx))

  nc <- NCOL(last_entry)
  new_lst <- new_list(NROW(last_entry))
  for (i in seq_len(NROW(last_entry))) {
    lst_i <- new_lst[[i]] <- as_list(last_entry[i, ])
    new_lst[[i]][[nc]] <- seq(lst_i[[nc]], by = tunit, length.out = n + 1)[-1]
    new_lst[[i]] <- as_tibble(new_lst[[i]])
  }
  out <- dplyr::bind_rows(!!! new_lst)
  if (keep_all) {
    out <- dplyr::bind_rows(.data[0L, ], out)
  }
  if (n == 1L) {
    int <- init_interval()
  } else {
    int <- interval(.data)
  }
  update_tsibble(out, .data, ordered = TRUE, interval = int)
}

#' Append rows to a tsibble
#'
#' Add new rows to the end of a tsibble by filling a key-index pair and `NA` for 
#' measured variables.
#'
#' `append_case()` is an alias of `append_row()`.
#'
#' @param .data A `tbl_ts`.
#' @param n An integer indicates the number of key-index pair to append.
#'
#' @seealso [new_data] for generating new observations to a tsibble
#' @rdname append-row
#' @export
#' @examples
#' tsbl <- tsibble(
#'   date = rep(as.Date("2017-01-01") + 0:2, each = 2), 
#'   group = rep(letters[1:2], 3),
#'   value = rnorm(6),
#'   key = id(group)
#' )
#' append_row(tsbl)
#' append_row(tsbl, n = 2)
append_row <- function(.data, n = 1L) {
  new_data <- new_data(.data, n = n)
  out <- dplyr::bind_rows(.data, new_data)
  ord <- is_ordered(.data)
  if (ord) ord <- NULL # re-order
  update_tsibble(out, .data, ordered = ord, interval = interval(.data))
}

#' @rdname append-row
#' @export
#' @usage NULL
append_case <- append_row
