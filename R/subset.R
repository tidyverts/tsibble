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
  res <- NextMethod()
  if (!is.data.frame(res)) return(res)

  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  # Ignore drop as an argument for counting
  n_real_args <- nargs() - !missing(drop)

  # Column or matrix subsetting if nargs() == 2L
  if (n_real_args <= 2L) {
    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL
  }

  cn <- names(res)
  nr <- vec_size(x)
  not_tsibble <- !(index_var(x) %in% cn) || vec_size(res) > nr || any(i > nr)
  if (not_tsibble) return(as_tibble(res))

  if (!is_null(i)) {
    if (is.numeric(i) && vec_duplicate_any(i) > 0) return(as_tibble(res))
  }

  new_key <- cn[cn %in% key_vars(x)]
  maybe_tsibble <- n_keys(x) > 1 && !all(is.element(key_vars(x), new_key))

  # Column subsetting only
  if (is_null(i) && !is_null(j) && maybe_tsibble) return(as_tibble(res))

  # TODO: Both row and column subsetting (not implemented for performance reason)
  # pedestrian[1:3, 2:5] # should return tsibble
  # pedestrian[1:14567, 2:5] # should return tibble
  # if (!is_null(i) && !is_null(j) && maybe_tsibble) return(as_tibble(res))

  if (index2_var(x) %in% names(res)) {
    build_tsibble(
      res,
      key = !!new_key, index = !!index(x), index2 = !!index2(x),
      interval = interval(x), ordered = is_ordered(x), validate = FALSE
    )
  } else { # index2 not there
    build_tsibble(
      res,
      key = !!new_key, index = !!index(x), interval = interval(x),
      ordered = is_ordered(x), validate = FALSE
    )
  }
}

#' @export
`[.grouped_ts` <- `[.tbl_ts`

#' @export
`$<-.tbl_ts` <- function(x, name, value) {
  if (name %in% c(key_vars(x), index_var(x))) {
    x <- as_tibble(x)
    return(NextMethod())
  }
  NextMethod()
}
