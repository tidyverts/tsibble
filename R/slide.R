#' Sliding window function
#'
#' @param x A vector of numerics, or data frame.
#' @param .f A function, or formula.
#' @param ... Additional arguments passed on to `.f`.
#' @param size Window size.
#' @param fill A single value or data frame to fill `NA`.
#'
#' @rdname slide
#' @export
#'
#' @examples
#' x <- sample(1:10, size = 100, replace = TRUE)
#' slide(x, sum, size = 10)
#' slide(x, sum, size = 10, fill = 0)
#' slide(x, ~ mean(.), size = 10)
slide <- function(x, .f, ..., size = 1, fill) {
  UseMethod("slide")
}

#' @rdname slide
#' @export
slide.numeric <- function(x, .f, ..., size = 1, fill = NA_real_) {
  lst_x <- slider(x, size = size)
  c(rep_len(fill, size - 1), purrr::map_dbl(lst_x, .f, ...))
}

#' @rdname slide
#' @param deframe TRUE a list is returned. FALSE returns a `tbl_ts`/`tbl_df`/`data.frame`.
#' @export
slide.data.frame <- function(
  x, .f, ..., size = 1, fill = data.frame(), deframe = TRUE
) {
  lst_x <- slider(x, size = size)
  result <- purrr::map(lst_x, .f, ...)
  output <- c(replicate(n = size - 1, fill, simplify = FALSE), result)
  if (deframe) {
    return(output)
  }
  # currently not support grouped_df/ts
  df <- dplyr::bind_rows(output)
  class(df) <- class(x)
  df
}

#' @rdname slide
#' @export
slider <- function(x, size = 1) {
  if (is_bare_list(x)) {
    abort("Unsupported input class: list")
  }
  if (!is_bare_numeric(size, n = 1) || size < 1) {
    abort("The window size must be a positive integer.")
  }
  if (is.data.frame(x)) {
    len_x <- nrow(x)
  } else {
    len_x <- length(x)
  }
  lst_idx <- seq_len(len_x - size + 1)
  if (is_atomic(x)) {
    return(purrr::map(lst_idx, ~ x[(.):(. + size - 1)]))
  }
  purrr::map(lst_idx, ~ x[(.):(. + size - 1), , drop = FALSE])
}

#' Tiling window function
#'
#' @param x A vector of numerics, or data frame.
#' @param .f A function, or formula.
#' @param ... Additional arguments passed on to `.f`.
#' @param size Window size.
#'
#' @rdname tile
#' @export
#'
#' @examples
#' tile(1:10, mean, size = 3)
tile <- function(x, .f, ..., size = 1) {
  UseMethod("tile")
}

#' @rdname tile
#' @export
tile.numeric <- function(x, .f, ..., size = 1) {
  lst_x <- tiler(x, size = size)
  purrr::map_dbl(lst_x, .f, ..., .default = NA_real_)
}

#' @rdname tile
#' @param deframe TRUE a list is returned. FALSE returns a `tbl_ts`/`tbl_df`/`data.frame`.
#' @export
tile.data.frame <- function(x, .f, ..., size = 1, deframe = TRUE) {
  lst_x <- tiler(x, size = size)
  if (deframe) {
    return(purrr::map(lst_x, .f, ...))
  }
  df <- purrr::map_df(lst_x, .f, ...)
  class(df) <- class(x)
  df
}

#' @rdname tile
#' @export
tiler <- function(x, size = 1) {
  if (is_bare_list(x)) {
    abort("Unsupported input class: list")
  }
  if (!is_bare_numeric(size, n = 1) || size < 1) {
    abort("The window size must be a positive integer.")
  }
  if (is.data.frame(x)) {
    seq_x <- nrow(x)
    denom <- seq_x + 1
  } else {
    seq_x <- seq_along(x)
    denom <- length(x) + 1
  }
  frac <- ceiling((seq_x %% denom) / size)
  unname(split(x, frac))
}
