rename_join_tsibble <- function(x, y, by = NULL, suffix = c(".x", ".y")) {
  intersect_names <- intersect(names(x), names(y))
  if (is_null(by)) {
    by <- intersect_names
  }
  common_names <- setdiff(intersect_names, by)
  if (!has_length(common_names)) return(x)

  names <- paste0(common_names, suffix[1])
  names(common_names) <- names
  rename_tsibble(x, !!! common_names)
}

#' @inheritParams dplyr::left_join
#' @rdname tsibble-tidyverse
#' @include dplyr-verbs.R
#' @export
left_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  x <- rename_join_tsibble(x, y, by = by, suffix = suffix)
  update_meta(tbl, x, ordered = is_ordered(x), interval = is_regular(x),
    validate = TRUE)
}

#' @rdname tsibble-tidyverse
#' @export
right_join.tbl_ts <- left_join.tbl_ts

#' @rdname tsibble-tidyverse
#' @export
inner_join.tbl_ts <- left_join.tbl_ts

#' @rdname tsibble-tidyverse
#' @export
full_join.tbl_ts <- left_join.tbl_ts

#' @rdname tsibble-tidyverse
#' @export
semi_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), interval = is_regular(x),
    validate = FALSE)
}

#' @rdname tsibble-tidyverse
#' @export
anti_join.tbl_ts <- semi_join.tbl_ts
