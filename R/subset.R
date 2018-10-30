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
  nr <- NROW(x)

  n_args <- nargs() - !missing(drop)

  if (n_args <= 2) { # only column subsetting
    if (!missing(drop)) {
      warn("`drop` is ignored.")
    }

    if (!missing(i)) { # x[1:2]
      i <- tidyselect::vars_select(names(x), i)
      lgl_i <- has_index(i, x) && has_any_key(i, x)
      result <- .subset(x, i)
      attr(result, "row.names") <- .set_row_names(nr)
      x <- remove_key(x, i)
      if (is_false(lgl_i)) {
        return(as_tibble(result))
      } else {
        return(build_tsibble_meta(
          result, key = key(x), index = !! index(x), index2 = !! index2(x),
          regular = is_regular(x), ordered = ordered
        ))
      }
    } else { # e.g. x[]
      result <- x
      attr(result, "row.names") <- .set_row_names(nr)
      return(build_tsibble_meta(
        result, key = key(x), index = !! index(x), index2 = !! index2(x),
        regular = is_regular(x), ordered = ordered, interval = interval(x)
      ))
    }
  }

  # subset by columns
  if (!missing(j)) {
    chr_j <- tidyselect::vars_select(names(x), j)
    lgl_j <- has_index(chr_j, x) && has_any_key(chr_j, x)
    if (is_false(lgl_j)) {
      return(NextMethod())
    }
    result <- .subset(x, j)
    x <- remove_key(x, chr_j)
  } else {
    result <- x
  }
  int <- interval(x)

  ordered <- is_ordered(x)
  if (!missing(i)) {
    # ordered <- row_validate(i)
    if (!is_empty(i)) {
      exceed_rows(result, max(i))
    }
    result <- purrr::map(result, `[`, i)
    nr <- length(result[[1]])
    if (!is_min_gap_one(i)) int <- NULL
  }

  if (drop) {
    if (has_length(result, 1)) {
      return(result[[1L]])
    } else if (nr == 1L) {
      return(result)
    }
  }

  # attr(result, "row.names") <- .set_row_names(nr)
  build_tsibble_meta(
    result, key = key(x), index = !! index(x), index2 = !! index2(x),
    regular = is_regular(x), ordered = ordered, interval = int
  )
}

is_index_null <- function(x) {
  if (is.null(index(x))) {
    abort("The `index` has been dropped somehow. Please reconstruct the `tbl_ts`.")
  }
}
