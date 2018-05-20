#' Lagged Differences in C++
#' 
#' @param x A numeric vector.
#' @param lag An integer indicating which lag to use.
#' @param differences An integer indicating the order of the difference.
#'
#' @return A numeric vector of the same length as `x` with padded `NA`.
#' @export
#' @examples
#' # examples from base
#' difference(1:10, 2)
#' difference(1:10, 2, 2)
#' x <- cumsum(cumsum(1:10))
#' difference(x, lag = 2)
#' difference(x, differences = 2)
difference <- function(x, lag = 1, differences = 1) {
  if (lag < 1 || differences < 1) {
    abort("`lag` and `differences` must be positive integers.");
  }

  diff_cpp(x, lag = lag, differences = differences)
}
