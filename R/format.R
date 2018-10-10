#' @export
print.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  is_index_null(x)
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
format.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  format(tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra))
}

#' @export
glimpse.tbl_ts <- function(x, width = NULL, ...) {
  idx <- index(x)
  t_span <- paste(range(dplyr::pull(x, !! idx), na.rm = TRUE), collapse = " ~ ")
  cat_line(t_span)
  build_tsibble_meta(
    NextMethod(), key = key(x), index = !! idx, index2 = !! index2(x), 
    groups = groups(x), ordered = is_ordered(x), regular = is_regular(x)
  )
  invisible(x)
}

#' @export
print.key <- function(x, ...) {
  cat_line(paste_comma(format(x, ...)))
  invisible(x)
}

#' @export
format.key <- function(x, ...) {
  if (is_empty(x)) return(list())
  reconstruct_key(
    x, 
    ~ purrr::map(purrr::map(., as.character), paste, collapse = " | "),
    ~ purrr::map(., as_string)
  )
}

#' @export
print.interval <- function(x, digits = NULL, ...) {
  cat_line(format(x, digits = digits, ...))
  invisible(x)
}

#' @export
format.interval <- function(x, digits = NULL, ...) {
  if (is_empty(x)) return("!")
  not_zero <- !purrr::map_lgl(x, function(x) x == 0)
  # if output contains all the zeros
  if (sum(not_zero) == 0) return("?")
  x <- translate_interval(x)
  output <- x[not_zero]
  paste0(output, names(output), collapse = " ")
}

translate_interval <- function(x) {
  set_names(
    x, 
    c(
      "Y", "Q", "M", "W", "D", 
      "h", "m", "s", "ms", ifelse(is_utf8_output(), "\U00B5s", "us"), "ns", ""
    )
  )
}
