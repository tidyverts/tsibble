group_split.tbl_ts <- function(.tbl, ..., keep = TRUE) {
  grped_ts <- group_by(.tbl, ...)
  group_split.grouped_ts(grped_ts, keep = keep)
}

group_split.grouped_ts <- function(.tbl, ..., keep = TRUE) {
  lst <- dplyr::group_split(as_tibble(.tbl), ..., keep = keep)
  if (keep) {
    lapply(lst, update_meta, .tbl, ordered = is_ordered(.tbl),
      interval = is_regular(.tbl))
  } else {
    first_data <- lst[[1]]
    left_over <- names(first_data)
    new_tsbl <- remove_key(.tbl, left_over)
    first_tsbl <- 
      remove_key(update_meta(first_data, new_tsbl, ordered = is_ordered(.tbl),
        interval = is_regular(.tbl)), left_over)
    lapply(lst, update_meta, first_tsbl, ordered = is_ordered(.tbl),
      interval = is_regular(.tbl))
  }
}

group_trim.grouped_ts <- function(.tbl, ...) {
  res <- dplyr::group_trim(as_tibble(.tbl), ...)
  update_meta(res, .tbl, ordered = is_ordered(.tbl), interval = interval(.tbl))
}
