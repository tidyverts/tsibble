#' @export
print.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  has_index(x)
  cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
  invisible(x)
}

#' @export
format.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  format(tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra))
}

#' @export
glimpse.tbl_ts <- function(x, width = NULL, ...) {
  as_tsibble(
    NextMethod(), id = key(x), index = !! index(x),
    validate = FALSE, regular = is_regular(x)
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
  if (is_empty(x)) {
    return("NULL")
  }
  nest_lgl <- is_nest(x)
  comb_keys <- purrr::map(x[!nest_lgl], as.character)
  if (any(nest_lgl)) {
    nest_keys <- as.character(flatten(x[nest_lgl]))
    cond_keys <- paste(nest_keys, collapse = " | ")
    comb_keys <- if (is_true(nest_lgl)) {
      cond_keys
    } else {
      c(cond_keys, comb_keys)
    }
  }
  names(comb_keys) <- comb_keys
  comb_keys
}

#' @export
print.vars <- print.key

#' @export
format.vars <- format.key

#' @export
print.interval <- function(x, digits = NULL, ...) {
  cat_line(format(x, digits = digits, ...))
  invisible(x)
}

#' @export
format.interval <- function(x, digits = NULL, ...) {
  if (is_empty(x)) {
    return("!")
  }
  if (is.null(digits)) {
    digits <- 3
  }
  not_zero <- !purrr::map_lgl(x, function(x) x == 0)
  # if output is empty, it means that duplicated time entries
  # if output is NA, it means that only one time entry
  output <- x[not_zero]
  vec_x <- round(rlang::flatten_dbl(output), digits = digits)
  if (is_empty(output) || is_empty(vec_x)) {
    return("?")
  }
  paste0(vec_x, toupper(names(output)), collapse = " ")
}
