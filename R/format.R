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
print.vars <- print.key

#' @export
format.vars <- format.key

#' @export
print.interval <- function(x, ...) {
  cat_line(format(x, ...))
  invisible(x)
}

#' @export
format.interval <- function(x, ...) {
  if (is_empty(x)) {
    return(surround("!", "["))
  }
  not_zero <- !purrr::map_lgl(x, function(x) x == 0)
  # if output is empty, it means that duplicated time entries
  # if output is NA, it means that only one time entry
  output <- x[not_zero]
  vec_x <- rlang::flatten_dbl(output)
  if (is_empty(output) || is_empty(vec_x)) {
    return(surround("?", "["))
  }
  paste0(vec_x, toupper(names(output)), collapse = " ")
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

