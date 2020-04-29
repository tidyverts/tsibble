#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype2 tbl_ts
#' @export
#' @export vec_ptype2.tbl_ts
vec_ptype2.tbl_ts <- function(x, y, ...) {
  UseMethod("vec_ptype2.tbl_ts", y)
}

#' @method vec_ptype2.tbl_ts tbl_ts
#' @export
vec_ptype2.tbl_ts.tbl_ts <- function(x, y, ...) {
  tsibble_ptype2(x, y)
}

vec_ptype2.tbl_ts.data.frame <- function(x, y, ...) {
  
}

vec_ptype2.grouped_ts.tbl_ts <- function(x, y, ...) {
  
}

vec_ptype2.grouped_ts.grouped_ts <- function(x, y, ...) {
  
}

vec_ptype2.grouped_ts.data.frame <- function(x, y, ...) {
  
}

tsibble_ptype2 <- function(x, y, ...) {
  idx_x <- index_var(x)
  key_x <- key_vars(x)
  if (is_tsibble(y)) {
    if (idx_x != index_var(y)) {
      abort("No common index variable for `x` and `y`.")
    }
    key_x <- union(key_x, key_vars(y))
  }
  lst <- vctrs:::tib_ptype2(x, y)
  build_tsibble_meta(lst, key_data = new_key_data(lst[key_x]),
    index = idx_x, index2 = idx_x, ordered = TRUE,
    interval = new_interval())
}

new_key_data <- function(x) {
  tibble(!!!x, !!".rows" := list_of(.ptype = integer()))
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_cast tbl_ts
#' @export
#' @export vec_cast.tbl_ts
vec_cast.tbl_ts <- function(x, to, ...) {
  UseMethod("vec_cast.tbl_ts")
}

#' @method vec_cast.tbl_ts tbl_ts
#' @export
vec_cast.tbl_ts.tbl_ts <- function(x, to, ...) {
  x
}
