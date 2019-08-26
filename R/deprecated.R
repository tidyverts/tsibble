#' Deprecated functions
#'
#' @description
#' * `is.tsibble()` \lifecycle{soft-deprecated}
#' * `as.tsibble()` \lifecycle{deprecated}
#' * `id()` \lifecycle{deprecated}
#' * `fill_na()` \lifecycle{defunct}
#'
#' @param x Other objects.
#' @param .data A tsibble.
#' @param ... Variables passed to tsibble()/as_tsibble().
#'
#' @rdname deprecated
#' @export
#' @keywords internal
as.tsibble <- function(x, ...) {
  .Deprecated("as_tsibble()")
  as_tsibble(x, ...)
}

#' @rdname deprecated
#' @export
#' @keywords internal
is.tsibble <- function(x) {
  lifecycle::deprecate_soft("0.8.4", "is.tsibble()", "is_tsibble()")
  is_tsibble(x)
}

#' @rdname deprecated
#' @export
#' @keywords internal
fill_na <- function(.data, ..., .full = FALSE) {
  lifecycle::deprecate_stop("0.8.1", "fill_na()", "fill_gaps()")
}

#' @rdname deprecated
#' @keywords internal
#' @export
id <- function(...) {
  unname(enexprs(...))
}

use_id <- function(x, key) {
  key_quo <- enquo(key)
  if (quo_is_null(key_quo)) {
    return(character())
  } else if (quo_is_call(key_quo)) {
    call_fn <- call_name(key_quo)
    if (call_fn == "id") {
      res <- eval_tidy(get_expr(key_quo), env = child_env(get_env(key_quo), id = id))
      header <- "`id()` is deprecated for creating key as of tsibble 0.8.0.\n"
      if (is_empty(res)) {
        res_vars <- NULL
        warn(sprintf("%sPlease use `key = NULL`.", header))
      } else if (has_length(res, 1)) {
        res_vars <- as_string(res[[1]])
        warn(sprintf("%sPlease use `key = %s`.", header, res_vars))
      } else {
        res_vars <- map(res, as_string)
        warn(sprintf("%sPlease use `key = c(%s)`.", header, comma(res_vars)))
      }
      return(res_vars)
    }
  }
  vars_select(names(x), !!key_quo)
}
