#' Internal vctrs methods
#'
#' These methods are the extensions that allow tsibble objects to
#' work with vctrs.
#'
#' @keywords internal
#' @name tsibble-vctrs
NULL

#' @rdname tsibble-vctrs
#' @method vec_ptype2 tbl_ts
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
  tsibble_ptype2(y, x, ...)
}

#' @export
vec_ptype2.tbl_ts.data.frame <- vec_ptype2.tbl_ts.tbl_ts

#' @export
vec_ptype2.tbl_df.tbl_ts <- vec_ptype2.data.frame.tbl_ts

#' @export
vec_ptype2.tbl_ts.tbl_df <- vec_ptype2.tbl_ts.tbl_ts

tsibble_ptype2 <- function(x, y, ...) {
  idx_x <- index_var(x)
  key_x <- key_vars(x)
  if (is_tsibble(y)) {
    if (idx_x != index_var(y)) {
      abort("No common index variable for `x` and `y`.")
    }
    key_x <- union(key_x, key_vars(y))
  }
  out <- df_ptype2(x, y, ...)
  build_tsibble_meta(out, key_data = new_key_data(out[key_x]),
    index = idx_x, index2 = idx_x, ordered = TRUE,
    interval = new_interval())
}

new_key_data <- function(x) {
  new_tibble(list2(!!!x, !!".rows" := list_of(.ptype = integer())), nrow = 0)
}

#' @rdname tsibble-vctrs
#' @method vec_cast tbl_ts
#' @export
vec_cast.tbl_ts <- function(x, to, ...) {
  UseMethod("vec_cast.tbl_ts")
}

#' @export
vec_cast.tbl_ts.tbl_ts <- function(x, to, ...) {
  is_identical <- identical(x, to)
  tbl <- tib_cast(x, to, ...)
  build_tsibble(tbl,
    key = key_vars(to),
    key_data = if (is_identical) key_data(x) else NULL,
    index = index_var(to), index2 = index2_var(to),
    ordered = is_ordered(to),
    validate = FALSE, .drop = key_drop_default(to))
}

#' @export
vec_cast.tbl_ts.tbl_df <- function(x, to, ...) {
  tbl <- tib_cast(x, to, ...)
  build_tsibble(tbl,
    key = key_vars(to), index = index_var(to), index2 = index2_var(to),
    ordered = TRUE, validate = TRUE, .drop = key_drop_default(to))
}

#' @export
vec_cast.tbl_ts.data.frame <- vec_cast.tbl_ts.tbl_df

#' @export
vec_cast.tbl_df.tbl_ts <- function(x, to, ...) {
  tib_cast(x, to, ...)
}

#' @export
vec_cast.data.frame.tbl_ts <- function(x, to, ...) {
  df_cast(x, to, ...)
}

#' @export
vec_restore.tbl_ts <- function(x, to, ..., n = NULL) {
  idx_var <- index_var(to)
  
  # During vctrs operations like list_unchop (used by unnest),
  # intermediate data structures may have NA in the index.
  # Return as tibble in these cases and let the operation complete.
  if (idx_var %in% names(x) && anyNA(x[[idx_var]])) {
    return(as_tibble(x))
  }
  
  # assuming `i` in order and no duplicates, minimal check for performance reason
  build_tsibble(x,
    key = key_vars(to), index = index_var(to), index2 = index2_var(to),
    ordered = TRUE, validate = FALSE, .drop = key_drop_default(to))
}

#' @export
vec_proxy.tbl_ts <- function(x, ...) {
  new_data_frame(x)
}

#' @export
vec_restore.grouped_ts <- function(x, to, ..., n = NULL) {
  x <- NextMethod()
  vec_restore.tbl_ts(x, to)
}

#' @export
vec_proxy.grouped_ts <- vec_proxy.tbl_ts
