#' Sliding window function
#'
#' @param x A vector of numerics
#' @param .f A function, formula, or atomic vector.
#' @param ... Additional arguments passed on to `.f`.
#' @param size Window size.
#' @param fill A single value to fill `NA`.
#'
#' @return A vector of numerics of the same length as `x`.
#' @export
#'
#' @examples
#' x <- 1:10
#' slide(x, sum, size = 3)
#' slide(x, sum, size = 3, fill = 0)
#' slide(x, ~ mean(.), size = 2)
slide <- function(x, .f, ..., size = 1, fill = numeric(0)) {
  .f <- purrr::as_mapper(.f, ...)
  results <- squash_dbl(slide_cpp(x, f = .f, size = size))
  if (has_length(fill)) {
    results <- case_na(results ~ fill)
  }
  return(results)
}
