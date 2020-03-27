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
