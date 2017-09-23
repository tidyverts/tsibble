#' @export
print.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
format.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  format(tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra))
}

#' @export
glimpse.tbl_ts <- function(x, width = NULL, ...) {
  NextMethod()
}

#' @export
print.key <- function(x, ...) {
  cat_line(format(x, ...))
  invisible(x)
}

#' @export
format.key <- function(x, ...) {
  if (is_empty(x)) {
    return(NULL)
  }
  nest_lgl <- is_nest(x)
  comb_keys <- paste(as.character(x[!nest_lgl]), collapse = ", ")
  if (any(nest_lgl)) {
    nest_keys <- as.character(purrr::flatten(x[nest_lgl]))
    cond_keys <- paste(nest_keys, collapse = " | ")
    comb_keys <- ifelse(
      is_true(nest_lgl),
      cond_keys,
      paste(cond_keys, comb_keys, sep = ", ")
    )
  }
  comb_keys
}

#' @export
print.interval <- function(x, ...) {
  cat_line(format(x, ...))
  invisible(x)
}

#' @export
format.interval <- function(x, ...) {
  not_zero <- !purrr::map_lgl(x, function(x) x == 0)
  output <- x[not_zero]
  paste0(rlang::flatten_dbl(output), toupper(names(output)), collapse = " ")
}

#' @export
print.index <- function(x, ...) {
  cat_line(format(x, ...))
  invisible(x)
}

#' @export
format.index <- function(x, ...) {
  f_text(x, width = 500L)
}

## helpers
# ref: tibble:::big_mark
big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) "." else ","
  formatC(x, big.mark = mark, ...)
}

# ref: tibble:::cat_line
cat_line <- function(...) {
  cat(paste0(..., "\n"), sep = "")
}

dim_tbl_ts <- function(x) {
  dim_x <- dim(x)
  format_dim <- purrr::map_chr(dim_x, big_mark)
  paste(format_dim, collapse = " x ")
}
