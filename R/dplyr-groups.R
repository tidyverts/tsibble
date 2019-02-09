#' @importFrom dplyr group_split
#' @export
dplyr::group_split

#' @export
group_split.grouped_ts <- function(.tbl, ..., keep = TRUE) {
  lst <- NextMethod()
  if (keep) {
    lapply(lst, update_tsibble, .tbl, ordered = is_ordered(.tbl))
  } else {
    first_data <- lst[[1]]
    left_over <- names(first_data)
    new_tsbl <- remove_key(.tbl, left_over)
    first_tsbl <- 
      remove_key(
        update_tsibble(first_data, new_tsbl, ordered = is_ordered(.tbl)),
        left_over
      )
    lapply(lst, update_tsibble, first_tsbl, ordered = is_ordered(first_tsbl))
  }
}

#' @importFrom dplyr group_trim
#' @export
dplyr::group_trim

#' @export
group_trim.grouped_ts <- function(.tbl, .drop = TRUE) {
  res <- group_trim(as_grouped_df(.tbl), .drop = .drop)
  update_tsibble(res, .tbl, ordered = is_ordered(.tbl), interval = interval(.tbl))
}
