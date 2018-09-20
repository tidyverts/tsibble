dont_know <- function(x, FUN) {
  cls <- class(x)[1]
  msg <- sprintf(
    "`%s()` doesn't know how to coerce the `%s` class yet.", FUN, cls
  )
  abort(msg)
}

unknown_interval <- function(x) {
  no_zeros <- !purrr::map_lgl(x, function(x) x == 0)
  if (sum(no_zeros) == 0) abort("Cannot deal with data of unknown interval.")
}

exceed_rows <- function(x, n = 1L) {
  nr <- NROW(x)
  if (n > nr) abort(sprintf("Must not exceed the rows (%i).", nr))
}

not_regular <- function(x) {
  if (!is_regular(x)) {
    abort("Can't handle `tbl_ts` of irregular interval.")
  }
}

suggest_key <- function(x) {
  sprintf("Can't create/coerce to a tsibble.\nDid you mean `key = id(%s)`?", x)
}

not_tsibble <- function(x) {
  if (is_false(is_tsibble(x) || inherits(x, "lst_ts"))) {
    abort(sprintf("%s is not a tsibble.", deparse(substitute(x))))
  }
}

check_valid_window <- function(.size, .align) {
  if (is_even(.size) && .align %in% c("c", "centre", "center")) {
    abort(sprintf(
      "Can't use `.align = %s` for even window `.size`.\nPlease use `.align = 'center-left'` or `.align = 'center-right'`.",
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

