#' Lagged differences
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams dplyr::lag
#' @param lag A positive integer indicating which lag to use.
#' @param differences A positive integer indicating the order of the difference.
#'
#' @return A numeric vector of the same length as `x`.
#' @seealso [dplyr::lead] and [dplyr::lag]
#' @export
#' @examples
#' # examples from base
#' difference(1:10, 2)
#' difference(1:10, 2, 2)
#' x <- cumsum(cumsum(1:10))
#' difference(x, lag = 2)
#' difference(x, differences = 2)
#' # Use order_by if data not already ordered (example from dplyr)
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)
#' scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#'
#' wrong <- mutate(scrambled, diff = difference(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, diff = difference(value, order_by = year))
#' arrange(right, year)
difference <- function(x, lag = 1, differences = 1, default = NA,
                       order_by = NULL) {
  if (lag < 1 || differences < 1) {
    abort("`lag` and `differences` must be positive integers.")
  }
  if (is_null(order_by)) {
    diff_impl(x, lag = lag, differences = differences, default = default)
  } else {
    with_order(order_by, diff_impl, x,
      lag = lag, differences = differences, default = default
    )
  }
}

diff_impl <- function(x, lag = 1, differences = 1, default = NA) {
  diff_x <- diff(x, lag = lag, differences = differences)
  vec_c(vec_rep(default, pmin(vec_size(x), lag * differences)), diff_x)
}
