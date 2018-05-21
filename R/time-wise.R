#' Lagged Differences
#' 
#' @param x A numeric vector.
#' @param lag An positive integer indicating which lag to use.
#' @param differences An positive integer indicating the order of the difference.
#' @param default Value used for non-existent rows, defaults to `NA`.
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
difference <- function(x, lag = 1, differences = 1, default = NA) {
  if (lag < 1 || differences < 1) {
    abort("`lag` and `differences` must be positive integers.");
  }

  diff_cpp(x, lag = lag, differences = differences, fill = default)
}
