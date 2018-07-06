#' Lagged differences
#' 
#' @param x A numeric vector.
#' @param lag An positive integer indicating which lag to use.
#' @param differences An positive integer indicating the order of the difference.
#' @param default Value used for non-existent rows, defaults to `NA`.
#' @param order_by Override the default ordering to use another vector.
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
#' tsbl <- tsibble(year = 2000:2005, value = (0:5) ^ 2, index = year)
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
    abort("`lag` and `differences` must be positive integers.");
  }
  if (!is.null(order_by)) {
    return(dplyr::with_order(order_by, diff_cpp, x, 
      lag = lag, differences = differences, fill = default
    ))
  }

  diff_cpp(x, lag = lag, differences = differences, fill = default)
}
