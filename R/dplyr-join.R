join_tsibble <- function(FUN, x, y, by = NULL, copy = FALSE, ...) {
  FUN <- match.fun(FUN, descend = FALSE)
  tbl_x <- as_tibble(x)
  tbl_y <- as_tibble(y, validate = FALSE)
  tbl <- FUN(x = tbl_x, y = tbl_y, by = by, copy = copy, ...)
  update_tsibble(tbl, x, ordered = is_ordered(x))
}

#' @inheritParams dplyr::left_join
#' @rdname tidyverse
#' @export
left_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(left_join, x, y, by = by, copy = copy, suffix = suffix, ...)
}


#' @rdname tidyverse
#' @export
right_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(right_join, x, y, by = by, copy = copy, suffix = suffix, ...)
}

#' @rdname tidyverse
#' @export
inner_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(inner_join, x, y, by = by, copy = copy, suffix = suffix, ...)
}

#' @rdname tidyverse
#' @export
full_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(full_join, x, y, by = by, copy = copy, suffix = suffix, ...)
}

#' @rdname tidyverse
#' @export
semi_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  join_tsibble(semi_join, x, y, by = by, copy = copy, ...)
}

#' @rdname tidyverse
#' @export
anti_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  join_tsibble(anti_join, x, y, by = by, copy = copy, ...)
}
