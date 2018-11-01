#' @export
`$.tbl_ts` <- function(x, i) {
  NextMethod()
}

#' @export
`$<-.tbl_ts` <- function(x, name, value) {
  name <- tidyselect::vars_select(names(x), name)
  if (is_null(value)) {
    lst_i <- map(name, ~ (.x = NULL))
  } else {
    lst_i <- map2(name, value, ~ (.x = .y))
  }
  mutate(x, !!! lst_i)
}

#' @export
`[[.tbl_ts` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
}

#' @export
`[[<-.tbl_ts` <- function(x, i, j, value) {
  n_args <- nargs()
  x <- ungroup(x)

  if (missing(i) && missing(j)) {
    abort("x[[i]] <- value : [[ ]] with missing subscript.")
  }

  if (n_args <= 3) { # x, i/j, value
    if (!missing(i)) { # x[[1:2]]
      i <- tidyselect::vars_select(names(x), i)
      if (is_null(value)) {
        lst_i <- map(i, ~ (.x = NULL))
      } else {
        lst_i <- map2(i, value, ~ (.x = .y))
      }
      mutate(x, !!! lst_i)
    }
  } else {
    exceed_rows(x, max(i))
    res <- x[i, ]
    j <- tidyselect::vars_select(names(x), j)
    lst_j <- map2(j, value, ~ (.x = .y))
    out <- rbind(mutate(res, !!! lst_j), x[-i, ])
    full_seq <- seq_len(NROW(x))
    orig_idx <- c(i, full_seq[-i])
    build_tsibble_meta(
      out[orig_idx, ], key = key(x), index = !! index(x), index2 = !! index2(x),
      regular = is_regular(x), ordered = is_ordered(x)
    )
  }
}

#' @export
`[.tbl_ts` <- function(x, i, j, drop = FALSE) {
  n_args <- nargs() - !missing(drop)

  if (n_args <= 2) { # only column subsetting
    if (!missing(drop)) {
      warn("`drop` is ignored.")
    }

    ordered <- is_ordered(x)
    if (!missing(i)) { # x[1:2]
      i <- tidyselect::vars_select(names(x), i)
      lgl_i <- has_index(i, x) && has_any_key(i, x)
      result <- .subset(x, i)
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

  build_tsibble_meta(
    result, key = key(x), index = !! index(x), index2 = !! index2(x),
    regular = is_regular(x), ordered = ordered, interval = int
  )
}

#' @export
`[<-.tbl_ts` <- function(x, i, j, value) {
  if (missing(i) && missing(j)) {
    abort("Oops! Do you need `as_tibble()`?")
  }

  n_args <- nargs()
  if (n_args <= 3) {
    x[[i]] <- value
  } else {
    x[[i, j]] <- value
  }
  x
}
