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
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = FALSE)
}

#' @importFrom dplyr right_join
#' @export
dplyr::right_join

#' @rdname tidyverse
#' @export
right_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = TRUE)
}

#' @importFrom dplyr inner_join
#' @export
dplyr::inner_join

#' @rdname tidyverse
#' @export
inner_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = TRUE)
}

#' @importFrom dplyr full_join
#' @export
dplyr::full_join

#' @rdname tidyverse
#' @export
full_join.tbl_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = TRUE)
}

#' @importFrom dplyr semi_join
#' @export
dplyr::semi_join

#' @rdname tidyverse
#' @export
semi_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = FALSE)
}

#' @importFrom dplyr anti_join
#' @export
dplyr::anti_join

#' @rdname tidyverse
#' @export
anti_join.tbl_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  tbl <- NextMethod()
  update_meta(tbl, x, ordered = is_ordered(x), validate = FALSE)
}
