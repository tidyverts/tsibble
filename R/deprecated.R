#' Deprecated functions
#'
#' @param x Other objects.
#' @rdname deprecated
#' @export
#' @keywords internal
as.tsibble <- function(x, ...) {
  as_tsibble(x, ...)
}

#' @rdname deprecated
#' @export
#' @keywords internal
pull_interval <- function(x) {
  .Deprecated("interval_pull()")
  UseMethod("interval_pull")
}

#' @rdname deprecated
#' @export
#' @keywords internal
#' @include gaps.R
fill_na <- function(.data, ..., .full = FALSE) {
  .Deprecated("fill_gaps()")
  fill_gaps(.data, ..., .full = .full)
}

#' Identifiers used for creating key
#'
#' @param ... Variables passed to tsibble()/as_tsibble().
#'
#' @rdname deprecated
#' @keywords internal
#' @export
id <- function(...) {
  unname(enexprs(...))
}

use_id <- function(x, key) {
  key_quo <- enquo(key)
  if (quo_is_call(key_quo)) {
    call_fn <- call_name(key_quo)
    if (call_fn == "id") {
      res <- eval_tidy(key_quo, env = child_env(get_env(key_quo), id = id))
      header <- "`id()` is deprecated for creating key.\n"
      if (is_empty(res)) {
        warn(sprintf("%sPlease use `key = NULL`.", header))
      } else if (has_length(res, 1)) {
        res_vars <- as_string(res[[1]])
        warn(sprintf("%sPlease use `key = %s`.", header, res_vars))
      } else {
        res_vars <- paste_comma(map(res, as_string))
        warn(sprintf("%sPlease use `key = c(%s)`.", header, res_vars))
      }
      return(res)
    }
  }
  vars_select(names(x), !! key_quo)
}
