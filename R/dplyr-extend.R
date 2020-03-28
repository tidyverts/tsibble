#' @export
dplyr_row_slice.tbl_ts <- function(data, i, ..., preserve = FALSE) {
  loc_df <- summarise(as_tibble(data), !!".loc" := list2(i))
  ascending <- all(map_lgl(loc_df[[".loc"]], validate_order))
  res <- NextMethod()
  if (preserve) {
    update_meta2(res, data, ordered = ascending, interval = interval(data))
  } else {
    update_meta(res, data, ordered = ascending, interval = interval(data))
  }
}

#' @export
dplyr_row_slice.grouped_ts <- dplyr_row_slice.tbl_ts

#' @export
dplyr_col_modify.tbl_ts <- function(data, cols) {
  mask <- TsibbleDataMask$new(data)
  data <- mask$retrieve_data()
  res <- NextMethod()

  idx_chr <- index_var(data)
  if (is_false(idx_chr %in% names(res))) { # index has been removed
    abort(sprintf(paste_inline(
      "Column `%s` (index) can't be removed for a tsibble.",
      "Do you need `as_tibble()` to work with data frame?"
    ), idx_chr))
  }

  vec_names <- names(cols)
  # either key or index is present in ...
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index(vec_names, data)
  if (val_idx) interval <- TRUE else interval <- interval(data)

  val_key <- has_any_key(vec_names, data)
  if (val_key) {
    key_vars <- setdiff(names(res), measured_vars(data))
    data <- remove_key(data, key_vars)
  }

  validate <- val_idx || val_key
  if (validate) {
    res <- retain_tsibble(res, key_vars(data), index(data))
  }
  build_tsibble(
    res,
    key = !!key_vars(data),
    key_data = if (val_key) NULL else key_data(data), index = !!index(data),
    index2 = !!index2(data), ordered = is_ordered(data), interval = interval,
    validate = FALSE, .drop = is_key_dropped(data)
  )
}

#' @export
dplyr_col_modify.grouped_ts <- dplyr_col_modify.tbl_ts

#' @export
dplyr_reconstruct.tbl_ts <- function(data, template) {
  update_meta(data, template,
    ordered = NULL, interval = is_regular(template),
    validate = TRUE)
}

#' @export
dplyr_reconstruct.grouped_ts <- dplyr_reconstruct.tbl_ts
