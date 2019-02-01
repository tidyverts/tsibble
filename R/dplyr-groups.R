#' @importFrom dplyr group_split
#' @export
dplyr::group_split

#' @export
group_split.tbl_ts <- function(.tbl, ..., keep = TRUE) {
  lst <- group_split(as_grouped_df(.tbl), ..., keep = keep)
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
group_trim.tbl_ts <- function(.tbl) {
  res <- group_trim(as_grouped_df(.tbl))
  update_tsibble(res, .tbl, ordered = is_ordered(.tbl), interval = interval(.tbl))
}

#' @importFrom dplyr group_map
#' @export
dplyr::group_map

#' @export
group_map.tbl_ts <- function(.tbl, .f, ...) {
  group_map(as_grouped_df(.tbl), .f, ...)
}

#' @importFrom dplyr group_walk
#' @export
dplyr::group_walk

#' @export
group_walk.tbl_ts <- function(.tbl, .f, ...) {
  group_walk(as_grouped_df(.tbl), .f, ...)
}
