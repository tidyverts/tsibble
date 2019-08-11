#' Deprecated functions
#'
#' \lifecycle{deprecated}
#'
#' @param x Other objects.
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
fill_na <- function(.data, ..., .full = FALSE) {
  .Defunct("fill_gaps()")
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
  if (quo_is_null(key_quo)) {
    return(character())
  } else if (quo_is_call(key_quo)) {
    call_fn <- call_name(key_quo)
    if (call_fn == "id") {
      res <- eval_tidy(get_expr(key_quo), env = child_env(get_env(key_quo), id = id))
      header <- "`id()` is deprecated for creating key.\n"
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

abort_stretch_size <- function(...) {
  dots <- dots_list(...)
  if (".size" %in% names(dots)) {
    abort("Argument `.size` is retired. Please use `.step`.")
  }
}

abort_gather <- function(..., pivot_longer = TRUE) {
  dots <- dots_list(...)
  if ("gather" %in% names(dots)) {
    abort("Argument `gather` is defunct, please use `pivot_longer` instead.")
  } else {
    pivot_longer
  }
}
