#' Sliding window function
#'
#' @param x A vector of numerics
#' @param .f A function, formula, or atomic vector.
#' @param ... Additional arguments passed on to `.f`.
#' @param size Window size.
#' @param fill A single value to fill `NA`.
#'
#' @return A vector of numerics of the same length as `x`.
#' @rdname slide
#' @export
#'
#' @examples
#' x <- 1:10
#' slide(x, sum, size = 3)
#' slide(x, sum, size = 3, fill = 0)
#' slide(x, ~ mean(.), size = 2)
slide <- function(x, .f, ..., size = 1, fill) {
  UseMethod("slide")
}

#' @rdname slide
#' @export
slide.default <- function(x, .f, ..., size = 1, fill = NA_real_) {
  rep_x <- rep_len(list(x), length(x) - (size - 1))
  lst_x <- purrr::imap(rep_x, ~ .x[.y:(.y + size - 1)])
  c(rep.int(fill, size - 1), purrr::map_dbl(lst_x, .f, ...))
}

#' @rdname slide
#' @export
slide.list <- function(x, .f, ..., size = 1, fill = list()) {
  .f <- purrr::as_mapper(.f, ...)
  results <- slide_cpp(x, f = .f, size = size, fill = fill)
  return(results)
}
