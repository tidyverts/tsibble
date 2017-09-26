#' @export
`[.tbl_ts` <- function(x, i, j, drop = FALSE) {
  if (drop) {
    warn("The argument 'drop' is ignored.")
  }

  # subset by columns
  if (!missing(j)) {
    lgl_j <- check_index_and_key(j, x = colnames(x), data = x)
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

# check if both index and key are present by selecting
# FALSE: not present otherwise return positions or names
check_index_and_key <- function(j, x, data) {
  if (is_false(check_index_var(j, x, data) && check_all_key(j, x, data))) {
    return(FALSE)
  }
  j
}

# check if either index or key is present by selecting
check_index_or_key <- function(j, x, data) {
  if (is_false(check_index_var(j, x, data) || check_all_key(j, x, data))) {
    return(FALSE)
  }
  j
}

check_index_var <- function(j, x, data) {
  result <- validate_vars(j, x = x)
  if (is_false(result)) {
    return(FALSE)
  }
  index <- f_text(index(data))
  if (index %in% result) {
    return(TRUE)
  } else {
    FALSE
  }
}

check_all_key <- function(j, x, data) {
  result <- validate_vars(j, x = x)
  if (is_false(result)) {
    return(FALSE)
  }
  key_vars <- flatten_key(key(data))
  if (all(key_vars %in% result)) {
    return(TRUE)
  } else {
    FALSE
  }
}

check_any_key <- function(j, x, data) {
  result <- validate_vars(j, x = x)
  if (is_false(result)) {
    return(FALSE)
  }
  key_vars <- flatten_key(key(data))
  if (any(key_vars %in% result)) {
    return(TRUE)
  } else {
    FALSE
  }
}
