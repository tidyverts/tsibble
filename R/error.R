is_index_null <- function(x) {
  if (is.null(index(x))) {
    abort("The `index` has been dropped somehow. Please reconstruct tsibble.")
  }
}

dont_know <- function(x, FUN) {
  cls <- class(x)[1]
  msg <- sprintf(
    "`%s()` doesn't know how to handle the %s class yet.", FUN, cls
  )
  abort(msg)
}

abort_unknown_interval <- function(x) {
  if (unknown_interval(x)) 
    abort("Can't proceed with tsibble of unknown interval.")
}

not_regular <- function(x) {
  if (!is_regular(x)) {
    abort("Can't handle tsibble of irregular interval.")
  }
}

suggest_key <- function(x) {
  sprintf("Key can only be created via `id()`.\nDid you mean `key = id(%s)`?", x)
}

not_tsibble <- function(x) {
  if (is_false(is_tsibble(x) || inherits(x, "lst_ts"))) {
    abort(sprintf("%s is not a tsibble.", deparse(substitute(x))))
  }
}

check_valid_window <- function(.size, .align) {
  if (is_even(.size) && .align %in% c("c", "centre", "center")) {
    abort(sprintf(
      "Can't use `.align = %s` for even window `.size`.\nPlease specify `.align = 'center-left'` or `.align = 'center-right'`.",
      .align
    ))
  }
}

abort_not_lst <- function (.x, .bind = FALSE) {
  if (!is.list(.x) && .bind) {
    abort(sprintf("`.bind = TRUE` only accepts list, not %s.", typeof(.x)))
  }
}

bad_window_function <- function(.size) {
  if (!is_integerish(.size, n = 1)) {
    abort("`.size` must be an integer.")
  }
  if (.size == 0) {
    abort("`.size` must not be 0.")
  }
}

