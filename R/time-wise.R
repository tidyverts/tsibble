#' Lagged differences
#'
#' @description
#' \lifecycle{stable}
#'
#' @param x A numeric vector.
#' @param lag An positive integer indicating which lag to use.
#' @param differences An positive integer indicating the order of the difference.
#' @param default Value used for non-existent rows, defaults to `NA`.
#' @param order_by Override the default ordering to use another vector.
#'
#' @return A numeric vector of the same length as `x`.
#' @seealso [dplyr::lead] and [dplyr::lag]
#' @export
#' @examples
#' # examples from base
#' difference(1:10, 2)
#' difference(1:10, 2, 2)
#' x <- cumsum(cumsum(1:10))
#' difference(x, lag = 2)
#' difference(x, differences = 2)
#' # Use order_by if data not already ordered (example from dplyr)
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)
#' scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#'
#' wrong <- mutate(scrambled, diff = difference(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, diff = difference(value, order_by = year))
#' arrange(right, year)
difference <- function(x, lag = 1, differences = 1, default = NA,
                       order_by = NULL) {
  if (lag < 1 || differences < 1) {
    abort("`lag` and `differences` must be positive integers.")
  }
  if (is_null(order_by)) {
    diff_impl(x, lag = lag, differences = differences, fill = default)
  } else {
    with_order(order_by, diff_impl, x,
      lag = lag, differences = differences, fill = default
    )
  }
}

diff_impl <- function(x, lag = 1, differences = 1, fill = NA) {
  diff_x <- diff(x, lag = lag, differences = differences)
  vec_c(vec_repeat(fill, lag * differences), diff_x)
}

lag_lead <- function(x, n = 1L, default = NA, order_by, op = "+") {
  if (n < 0 || !is_integerish(n, n = 1)) {
    abort("`n` must be a non-negative integer.")
  }
  if (op == "+") {
    idx <- vec_match(order_by + n, order_by) # tlead()
  } else {
    idx <- vec_match(order_by - n, order_by) # tlag()
  }
  idx_na <- vec_in(idx, NA)
  x <- vec_slice(x, idx)
  vec_slice(x, idx_na) <- default
  x
}

tlag <- function(x, n = 1L, default = NA, order_by) {
  lag_lead(x, n, default, order_by, "-")
}

tlead <- function(x, n = 1L, default = NA, order_by) {
  lag_lead(x, n, default, order_by, "+")
}

keyed_lag <- function(var, n = 1L, default = NA) {
  data <- peek_tsibble_mask()
  col <- sym(names(eval_select(expr({{ var }}), data)))
  tunits <- default_time_units(interval(data))
  idx_chr <- index_var(data)
  grped_df <- new_grouped_df(data, groups = key_data(data))
  res_df <- mutate(grped_df,
    !!idx_chr := tlag(!!col, n = n * tunits, default, !!index(data)))
  res_df[[idx_chr]]
}

keyed_lead <- function(var, n = 1L, default = NA) {
  data <- peek_tsibble_mask()
  col <- sym(names(eval_select(expr({{ var }}), data)))
  tunits <- default_time_units(interval(data))
  idx_chr <- index_var(data)
  grped_df <- new_grouped_df(data, groups = key_data(data))
  res_df <- mutate(grped_df,
    !!idx_chr := tlead(!!col, n = n * tunits, default, !!index(data)))
  res_df[[idx_chr]]
}
