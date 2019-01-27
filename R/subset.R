#' @export
`$.tbl_ts` <- function(x, i) {
  NextMethod()
}

# #' @export
# `$<-.tbl_ts` <- function(x, name, value) {
#   exceed_rows(x, length(value))
#   name <- tidyselect::vars_select(union(names(x), name), name)
#   lst_i <- map(name, ~ (.x = value))
#   mutate(x, !!! lst_i)
# }

#' @export
`[[.tbl_ts` <- function(x, i, j, ..., exact = TRUE) {
  NextMethod()
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
      lgl_i <- has_index(i, x) && (n_keys(x) < 2 || has_any_key(i, x))
      result <- .subset(x, i)
      x <- remove_key(x, i)
      if (is_false(lgl_i)) {
        return(as_tibble(NextMethod()))
      } else {
        return(build_tsibble(
          result, key = key(x), index = !! index(x), index2 = !! index2(x),
          regular = is_regular(x), ordered = ordered, validate = FALSE
        ))
      }
    } else { # e.g. x[]
      result <- x
      return(build_tsibble(
        result, key = key(x), index = !! index(x), index2 = !! index2(x),
        regular = is_regular(x), ordered = ordered, interval = interval(x),
        validate = FALSE
      ))
    }
  }

  # subset by columns
  if (!missing(j)) {
    chr_j <- tidyselect::vars_select(names(x), j)
    lgl_j <- has_index(chr_j, x) && (n_keys(x) < 2 || has_any_key(chr_j, x))
    result <- .subset(x, j)
    if (is_false(lgl_j)) {
      return(as_tibble(NextMethod()))
    }
    x <- remove_key(x, chr_j)
  } else {
    result <- x
  }

  ordered <- is_ordered(x)
  if (!missing(i)) {
    # ordered <- row_validate(i)
    result <- purrr::map(result, `[`, i)
    if (any(i > NROW(x))) {
      return(as_tibble(NextMethod()))
    }
    nr <- length(result[[1]])
  }

  if (drop) {
    if (has_length(result, 1)) {
      return(result[[1L]])
    } else if (nr == 1L) {
      return(result)
    }
  }

  build_tsibble(
    result, key = key(x), index = !! index(x), index2 = !! index2(x),
    regular = is_regular(x), ordered = ordered, interval = NULL,
    validate = FALSE
  )
}

# #' @export
# `[<-.tbl_ts` <- function(x, i, j, value) {
#   if (missing(i) && missing(j)) {
#     abort("Oops! Do you need tsibble?")
#   }
#
#   n_args <- nargs()
#   x <- ungroup(x)
#   exceed_rows(x, length(value))
#
#   if (n_args <= 3 && !missing(i)) { # x, i/j, value
#     # x[i] <- 
#     # x[, j] <- 
#     if (i > NCOL(x)) {
#       i <- as.character(i)
#       i <- tidyselect::vars_select(union(i, names(x)), i)
#     } else {
#       i <- tidyselect::vars_select(names(x), i)
#     }
#     lst_i <- map(i, ~ (.x = value))
#     mutate(x, !!! lst_i)
#   } else { # x[i, j] <- 
#     if (missing(i)) {
#       i <- seq_len(NROW(x))
#       res <- x
#     } else {
#       exceed_rows(x, max(i))
#       res <- x[i, ]
#     }
#     if (j > NCOL(x)) { # character always greater than numbers
#       j <- as.character(j)
#       j <- tidyselect::vars_select(union(j, names(x)), j)
#     } else {
#       j <- tidyselect::vars_select(names(x), j)
#     }
#     lst_j <- map(j, ~ (.x = value))
#     out <- rbind.data.frame(mutate(res, !!! lst_j), x[-i, ])
#     full_seq <- seq_len(NROW(x))
#     orig_idx <- order(c(i, full_seq[-i]))
#     build_tsibble_meta(
#       out[orig_idx, ], key = key(x), index = !! index(x), index2 = !! index2(x),
#       regular = is_regular(x), ordered = is_ordered(x)
#     )
#   }
# }
