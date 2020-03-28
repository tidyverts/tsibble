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

  cn <- names(res)
  new_key <- cn[cn %in% key_vars(x)]

  if (!missing(i)) {
    if (is.numeric(i) && vec_duplicate_any(i) > 0) return(as_tibble(res))
  }

  not_tsibble <- !(index_var(x) %in% cn) || vec_size(res) > vec_size(x)
  maybe_tsibble <- n_keys(x) > 1 && !all(is.element(key(x), new_key))
  if (not_tsibble || maybe_tsibble) {
    as_tibble(res)
  } else if (index2_var(x) %in% names(res)) {
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
