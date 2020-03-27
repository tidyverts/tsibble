#' @method rbind tbl_ts
#' @export
rbind.tbl_ts <- function(...) {
  is_identical <- function(x, y) {
    if (all(is.element(x, y))) x else FALSE
  }
  lst <- list2(...)
  lst_key <- map(lst, key_vars)
  diff_key <- is_false(reduce(lst_key, is_identical))
  if (diff_key) {
    abort("Can't row-bind tsibbles of different keys.")
  }
  lst_index <- map(lst, index_var)
  diff_index <- is_false(reduce(lst_index, is_identical))
  if (diff_index) {
    abort("Can't row-bind tsibbles of different indexes.")
  }
  lgl_reg <- map_lgl(lst, function(x) (x %@% "interval") %@% ".regular")
  if (all(lgl_reg) != any(lgl_reg)) {
    abort("Can't row-bind tsibbles of different regularities.")
  }
  x <- vec_rbind(...)
  x <- retain_tsibble(x, lst_key[[1]], sym(lst_index[[1]]))
  build_tsibble(x,
    key = !!lst_key[[1]], index = !!lst_index[[1]],
    ordered = NULL, interval = lgl_reg[1], validate = FALSE
  )
}

#' @method cbind tbl_ts
#' @export
cbind.tbl_ts <- function(...) {
  bind_cols(...)
}
