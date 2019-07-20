#' @export
`$.tbl_ts` <- function(x, i) {
  NextMethod()
}

# #' @export
# `$<-.tbl_ts` <- function(x, name, value) {
#   exceed_rows(x, length(value))
#   name <- vars_select(union(names(x), name), name)
#   lst_i <- map(name, ~ (.x = value))
#   mutate(x, !!! lst_i)
# }

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
    if (anyDuplicated.default(i) > 0) return(as_tibble(res))
  }

  not_tsibble <- !(index_var(x) %in% cn) || NROW(res) > NROW(x)
  maybe_tsibble <- n_keys(x) > 1 && !all(is.element(key(x), new_key))
  if (not_tsibble || maybe_tsibble) {
    as_tibble(res)
  } else if (index2_var(x) %in% names(res)) {
    build_tsibble(
      res,
      key = !!new_key, index = !!index(x), index2 = !!index2(x),
      interval = is_regular(x), ordered = is_ordered(x), validate = FALSE
    )
  } else { # index2 not there
    build_tsibble(
      res,
      key = !!new_key, index = !!index(x), interval = is_regular(x),
      ordered = is_ordered(x), validate = FALSE
    )
  }
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
#       i <- vars_select(union(i, names(x)), i)
#     } else {
#       i <- vars_select(names(x), i)
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
#       j <- vars_select(union(j, names(x)), j)
#     } else {
#       j <- vars_select(names(x), j)
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
