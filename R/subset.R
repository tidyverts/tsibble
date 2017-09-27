#' @export
`[.tbl_ts` <- function(x, i, j, drop = FALSE) {
  if (drop) {
    warn("The argument 'drop' is ignored.")
  }

  # subset by columns
  if (!missing(j)) {
    chr_j <- validate_vars(j, colnames(x))
    lgl_j <- has_index_var(chr_j, x) && has_all_key(chr_j, x)
    if (is_false(lgl_j)) {
      return(NextMethod())
    }
    result <- .subset(x, j)
  } else {
    result <- x
  }

  if (!missing(i)) {
    result <- purrr::map(result, `[`, i)
  }

  index <- f_rhs(index(x))
  as_tsibble.tbl_df(
    result, !!! key(x), index = !! index,
    validate = FALSE, regular = is_regular(x)
  )
}

# this function usually follows validate_vars()
has_index_var <- function(j, x) {
  index <- f_text(index(x))
  index %in% j
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
  idx_name <- format(x)
  idx_idx <- match(idx_name, rhs)
  new_idx_name <- if (is.na(idx_idx)) {
    idx_name
  } else {
    lhs[idx_idx]
  }
  new_idx_name # character
}
