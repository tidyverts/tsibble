rename_join_tsibble <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  intersect_names <- intersect(names(x), names(y))
  if (is_null(by)) {
    by <- intersect_names
  }
  common_names <- setdiff(intersect_names, by)
  if (has_length(common_names)) {
    names <- paste0(common_names, suffix[1])
    names(common_names) <- names
    val_names <- vars_rename(names(x), !!! common_names)
    x <- rename_index2(rename_index(x, val_names), val_names)
    x <- rename_key(x, val_names)
  }
  x
}

#' @inheritParams dplyr::left_join
#' @rdname tsibble-tidyverse
#' @include dplyr-verbs.R
left_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  x <- rename_join_tsibble(x, y, by = by, suffix = suffix)
  update_meta(tbl, x, ordered = is_ordered(x), validate = TRUE)
}

#' @rdname tsibble-tidyverse
right_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  x <- rename_join_tsibble(x, y, by = by, suffix = suffix)
  update_meta(tbl, x, ordered = is_ordered(x), validate = TRUE)
}

#' @rdname tsibble-tidyverse
inner_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  x <- rename_join_tsibble(x, y, by = by, suffix = suffix)
  update_meta(tbl, x, ordered = is_ordered(x), validate = TRUE)
}

#' @rdname tsibble-tidyverse
full_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  x <- rename_join_tsibble(x, y, by = by, suffix = suffix)
  update_meta(tbl, x, ordered = is_ordered(x), validate = TRUE)
}

#' @rdname tsibble-tidyverse
semi_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = FALSE)
}

#' @rdname tsibble-tidyverse
anti_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = FALSE)
}
