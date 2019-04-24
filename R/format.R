#' @export
format.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  is_index_null(x)
  if (!is_null(x %@% "regular") || !is_null(x %@% "ordered")) {
    warn("`.data`. is a corrupt tsibble object, please reconstruct with `as_tsibble()`.")
  }
  format(tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra))
}

#' @export
print.interval <- function(x, digits = NULL, ...) {
  cat_line(format(x, digits = digits, ...))
  invisible(x)
}

#' @export
format.interval <- function(x, digits = NULL, ...) {
  if (is_empty(x)) return("!")
  not_zero <- !map_lgl(x, function(x) x == 0)
  # if output contains all the zeros
  if (sum(not_zero) == 0) return("?")
  x <- translate_interval(x)
  output <- x[not_zero]
  paste0(output, names(output), collapse = " ")
}

translate_interval <- function(x) {
  names_unit <- fn_fmls_names(init_interval)
  set_names(
    x[names_unit], 
    c(
      "Y", "Q", "M", "W", "D", "h", "m", "s", "ms", 
      ifelse(is_utf8_output(), "\U00B5s", "us"), "ns", ""
    )
  )
}
