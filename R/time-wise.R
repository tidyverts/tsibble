#' Lagged Differences in C++
#' 
#' @param x A numeric vector.
#' @param lag An integer indicating which lag to use.
#' @param differences An integer indicating the order of the difference.
#'
#' @return A numeric vector of the same size of `x` with `NA` padding in.
#' @export
#' @examples
#' # examples from base
#' diff(1:10, 2)
#' diff(1:10, 2, 2)
#' x <- cumsum(cumsum(1:10))
#' diff(x, lag = 2)
#' diff(x, differences = 2)
diff <- function(x, lag = 1, differences = 1) {
  diff_cpp(x, lag = lag, differences = differences)
}
