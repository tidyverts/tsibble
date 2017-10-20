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
  if (size < 1) {
    abort("The window size must be a positive integer.")
  }
  UseMethod("slide")
}

#' @rdname slide
#' @export
slide.numeric <- function(x, .f, ..., size = 1, fill = NA_real_) {
  rep_x <- rep_len(list(x), length(x) - (size - 1))
  lst_x <- purrr::imap(rep_x, ~ .x[.y:(.y + size - 1)])
  c(rep_len(fill, size - 1), purrr::map_dbl(lst_x, .f, ..., .default = fill))
}

#' @rdname slide
#' @export
slide.list <- function(x, .f, ..., size = 1, fill = list()) {
  lst_idx <- seq_len(length(x) - size + 1)
  lst_x <- purrr::map(lst_idx, ~ x[(.):(. + size - 1), , drop = FALSE])
  result <- purrr::map(lst_x, .f, ..., .default = fill)
  c(replicate(n = size - 1, fill, simplify = FALSE), result)
}

#' @rdname slide
#' @param deframe TRUE a list is returned. FALSE returns a `tbl_ts`/`tbl_df`/`data.frame`.
#' @export
slide.data.frame <- function(
  x, .f, ..., size = 1, fill = data.frame(), deframe = TRUE
) {
  lst_idx <- seq_len(nrow(x) - size + 1)
  lst_x <- purrr::map(lst_idx, ~ x[(.):(. + size - 1), , drop = FALSE])
  result <- purrr::map(lst_x, .f, ...)
  output <- c(replicate(n = size - 1, fill, simplify = FALSE), result)
  if (deframe) {
    return(output)
  }
  df <- dplyr::bind_rows(output)
  class(df) <- class(x)
  df
}
