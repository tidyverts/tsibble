index_by <- function(.data, ...) {
  exprs <- enexprs(..., .named = TRUE)
  if (is_false(has_length(exprs, 1))) {
    abort("`index_by()` only accepts one expression")
  }
  attr(.data, "idx") <- exprs
  .data
}
