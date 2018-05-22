#' @export
`$.tbl_ts` <- function(x, i) {
  NextMethod()
}

#' @export
`[[.tbl_ts` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
}

#' @export
`[.tbl_ts` <- function(x, i, j, drop = FALSE) {
  if (drop) {
    warn("The argument 'drop' is ignored.")
  }

  result <- x
  # subset by columns
  if (!missing(j)) {
    chr_j <- validate_vars(j, colnames(x))
    lgl_j <- has_index(chr_j, x) && has_distinct_key(chr_j, x)
    if (is_false(lgl_j)) {
      return(NextMethod())
    }
    result <- .subset(x, j)
    x <- key_reduce(x, chr_j)
  }

  ordered <- is_ordered(x)
  if (!missing(i)) {
    ordered <- row_validate(i)
    result <- purrr::map(result, `[`, i)
  }

  build_tsibble(
    result, key = key(x), index = !! index(x), index2 = !! index2(x),
    validate = FALSE, regular = is_regular(x), ordered = ordered
  )
}

is_index_null <- function(x) {
  if (is.null(index(x))) {
    abort("The `index` has been dropped somehow. Please reconstruct the `tbl_ts`.")
  }
}

# this function usually follows validate_vars()
has_index <- function(j, x) {
  is_index_null(x)
  index <- c(quo_name(index(x)), quo_name(index2(x)))
  any(index %in% j)
}

has_distinct_key <- function(j, x) {
  key_vars <- key_flatten(key_distinct(key(x)))
  all(key_vars %in% j)
}

has_any_key <- function(j, x) {
  key_vars <- key_flatten(key(x))
  any(key_vars %in% j)
}
