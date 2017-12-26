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
    lgl_j <- has_index_var(chr_j, x) && has_reduce_key(chr_j, x)
    if (is_false(lgl_j)) {
      return(NextMethod())
    }
    result <- .subset(x, j)
    key(x) <- update_key(key(x), chr_j)
  }

  if (!missing(i)) {
    result <- purrr::map(result, `[`, i)
  }

  as_tsibble(
    result, key = key(x), index = !! index(x),
    validate = FALSE, regular = is_regular(x)
  )
}

# this function usually follows validate_vars()
has_index_var <- function(j, x) {
  index <- f_text(index(x))
  index %in% j
}

has_reduce_key <- function(j, x) {
  key_vars <- flatten_key(reduce_key(key(x)))
  all(key_vars %in% j)
}

has_all_key <- function(j, x) {
  key_vars <- flatten_key(key(x))
  all(key_vars %in% j)
}

has_any_key <- function(j, x) {
  key_vars <- flatten_key(key(x))
  any(key_vars %in% j)
}

update_index <- function(x, rhs, lhs) {
  as_quosure(sym(update_index_name(x, rhs, lhs)))
}

update_index_name <- function(x, rhs, lhs) {
  idx_name <- quo_text2(x)
  idx_idx <- match(idx_name, rhs)
  new_idx_name <- lhs[idx_idx]
  if (is.na(idx_idx)) {
    new_idx_name <- idx_name
  }
  new_idx_name # character
}
