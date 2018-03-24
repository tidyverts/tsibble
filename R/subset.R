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
    x <- key_reduce(x, chr_j)
  }

  if (!missing(i)) {
    result <- purrr::map(result, `[`, i)
  }

  build_tsibble(
    result, key = key(x), index = !! index(x),
    validate = FALSE, regular = is_regular(x), ordered = is_ordered(x)
  )
}

has_index <- function(x) {
  if (is.null(index(x))) {
    abort("The `index` has been dropped somehow. Please reconstruct the `tbl_ts`.")
  }
}

# this function usually follows validate_vars()
has_index_var <- function(j, x) {
  has_index(x)
  index <- as.character(index(x))
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

index_rename <- function(.data, ...) {
  quos <- enquos(...)
  idx <- index(.data)
  rhs <- purrr::map_chr(quos, quo_get_expr)
  lhs <- names(rhs)
  idx_chr <- quo_text2(idx)
  idx_pos <- match(idx_chr, rhs)
  new_idx_chr <- lhs[idx_pos]
  if (is.na(idx_pos)) {
    new_idx_chr <- idx_chr
  }
  dat_idx_pos <- match(idx_chr, names(.data))
  names(.data)[dat_idx_pos] <- new_idx_chr
  build_tsibble(
    .data, key = key(.data), index = !! sym(new_idx_chr), 
    regular = is_regular(.data), validate = FALSE, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}
