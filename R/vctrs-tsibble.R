#' @export
vec_ptype2.tbl_ts <- function(x, y, ...) {
  UseMethod("vec_ptype2.tbl_ts", y)
}

#' @export
vec_ptype2.tbl_ts.tbl_ts <- function(x, y, ...) {
  tsibble_ptype2(x, y, ...)
}

#' @export
vec_ptype2.data.frame.tbl_ts <- function(x, y, ...) {
  vctrs:::df_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tbl_ts.data.frame <- vec_ptype2.data.frame.tbl_ts

#' @export
vec_ptype2.tbl_df.tbl_ts <- function(x, y, ...) {
  vctrs:::tib_ptype2(x, y, ...)
}

#' @export
vec_ptype2.tbl_ts.tbl_df <- vec_ptype2.tbl_df.tbl_ts

tsibble_ptype2 <- function(x, y, ...) {
  idx_x <- index_var(x)
  key_x <- key_vars(x)
  if (is_tsibble(y)) {
    if (idx_x != index_var(y)) {
      abort("No common index variable for `x` and `y`.")
    }
    key_x <- union(key_x, key_vars(y))
  }
  out <- vctrs:::tib_ptype2(x, y, ...)
  build_tsibble_meta(out, key_data = new_key_data(out[key_x]),
    index = idx_x, index2 = idx_x, ordered = TRUE,
    interval = new_interval())
}

new_key_data <- function(x) {
  tibble(!!!x, !!".rows" := list_of(.ptype = integer()))
}

#' @export
vec_cast.tbl_ts <- function(x, to, ...) {
  UseMethod("vec_cast.tbl_ts")
}

#' @export
vec_cast.tbl_ts.tbl_ts <- function(x, to, ...) {
  is_identical <- identical(x, to)
  tbl <- vctrs:::tib_cast(x, to, ...)
  build_tsibble(tbl,
    key = key_vars(to),
    key_data = if (is_identical) key_data(x) else NULL,
    index = index_var(to), index2 = index2_var(to),
    ordered = is_ordered(to),
    validate = FALSE, .drop = key_drop_default(to))
}

#' @export
vec_cast.tbl_ts.tbl_df <- vec_cast.tbl_ts.tbl_ts

#' @export
vec_cast.tbl_ts.data.frame.tbl_ts <- vec_cast.tbl_ts.tbl_ts

#' @export
vec_cast.tbl_df.tbl_ts <- function(x, to, ...) {
  vctrs:::tib_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.tbl_ts <- function(x, to, ...) {
  vctrs:::df_cast(x, to, ...)
}
