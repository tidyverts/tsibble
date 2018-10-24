#' @inheritParams dplyr::left_join
#' @name tidyverse
#' @rdname tidyverse
#' @include dplyr-verbs.R
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
  join_tsibble(right_join, x, y, by = by, copy = copy, suffix = suffix, 
    validate = TRUE, ...)
}

#' @rdname tidyverse
#' @export
inner_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(inner_join, x, y, by = by, copy = copy, suffix = suffix, 
    validate = TRUE, ...)
}

#' @rdname tidyverse
#' @export
full_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(full_join, x, y, by = by, copy = copy, suffix = suffix, 
    validate = TRUE, ...)
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
