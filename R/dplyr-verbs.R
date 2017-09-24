#' @seealso [dplyr::filter]
# ToDo: filter(pkgs_ts, ~ year() == 2016)? => tbl_ts
# ToDo: filter(pkgs_ts, ~ month() == 1)? => tbl_df
filter.tbl_ts <- function(.data, ...) {
  key <- key(.data)
  index <- index(.data)
  interval <- interval(.data)
  cls <- class(.data)
  .data <- NextMethod()
  return(structure(
    .data, key = key, index = index, interval = interval, class = cls
  ))
}

#' @seealso [dplyr::select]
# ToDo: select should work with everything(), ends_with() and etc. too
select.tbl_ts <- function(.data, ...) {
  cls <- class(.data)
  key <- key(.data)
  index <- index(.data)
  interval <- interval(.data)
  .data <- NextMethod()
  dots_cap <- quos(...)
  idx_there <- any(purrr::map_lgl(dots_cap, function(x) x == index))
  key_there <- any(rlang::flatten_lgl(purrr::map(key, function(x)
    purrr::map_lgl(dots_cap, function(y) y == x)
  )))
  if (idx_there && key_there) {
    return(structure(
      .data, key = key, index = index, interval = interval, class = cls
    ))
  } else {
    return(structure(.data, class = c("tbl_df", "tbl", "data.frame")))
  }
}

#' @seealso [dplyr::mutate]
mutate.tbl_ts <- function(.data, ...) {
  key <- key(.data)
  index <- index(.data)
  interval <- interval(.data)
  cls <- class(.data)
  .data <- NextMethod()
  return(structure(
    .data, key = key, index = index, interval = interval, class = cls
  ))
}

#' @seealso [dplyr::group_by]
#' @export
group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  key <- key(.data)
  index <- index(.data)
  regular <- is_regular(.data)
  idx_var <- format(index)
  grped_chr <- key_chr(.data, ...)
  if (idx_var %in% grped_chr) {
    abort(paste("The index variable", surround(idx_var), "cannot be grouped."))
  }

  grped_sym <- syms(grped_chr)
  class(.data) <- c("tbl_df", "tbl", "data.frame")
  .data <- .data %>% 
    group_by(!!! grped_sym, add = add)

  as_tsibble(
    .data, !!! key, index = !! f_rhs(index), validate = FALSE, regular = regular
  )
}
