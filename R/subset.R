#' @export
`[.tbl_ts` <- function(x, i, j, drop = FALSE) {
  if (drop) {
    warn("The argument 'drop' is ignored.")
  }

  # subset by columns
  if (!missing(j)) {
    lgl_j <- check_index_and_key(j, x)
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

check_index_and_key <- function(j, x) {
  if (is_false(check_index_var(j, x) && check_key_var(j, x))) {
    return(FALSE)
  }
  j
}

check_index_var <- function(j, x) {
  result <- validate_vars(j, x = x)
  if (is_false(result)) {
    return(FALSE)
  }
  index <- f_text(index(x))
  if (index %in% result) {
    return(j)
  } else {
    FALSE
  }
}

check_key_var <- function(j, x) {
  result <- validate_vars(j, x = x)
  if (is_false(result)) {
    return(FALSE)
  }
  key_vars <- flatten_key(key(x))
  if (all(key_vars %in% result)) {
    return(j)
  } else {
    FALSE
  }
}
