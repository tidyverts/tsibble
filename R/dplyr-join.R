#' @importFrom dplyr left_join
#' @export
dplyr::left_join

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

#' @importFrom dplyr right_join
#' @export
dplyr::right_join

#' @rdname tidyverse
#' @export
right_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(right_join, x, y, by = by, copy = copy, suffix = suffix, 
    validate = TRUE, ...)
}

#' @importFrom dplyr inner_join
#' @export
dplyr::inner_join

#' @rdname tidyverse
#' @export
inner_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(inner_join, x, y, by = by, copy = copy, suffix = suffix, 
    validate = TRUE, ...)
}

#' @importFrom dplyr full_join
#' @export
dplyr::full_join

#' @rdname tidyverse
#' @export
full_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  join_tsibble(full_join, x, y, by = by, copy = copy, suffix = suffix, 
    validate = TRUE, ...)
}

#' @importFrom dplyr semi_join
#' @export
dplyr::semi_join

#' @rdname tidyverse
#' @export
semi_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  join_tsibble(semi_join, x, y, by = by, copy = copy, ...)
}

#' @importFrom dplyr anti_join
#' @export
dplyr::anti_join

#' @rdname tidyverse
#' @export
anti_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  join_tsibble(anti_join, x, y, by = by, copy = copy, ...)
}
