slide_by <- function(.data, .size = 1, .step = 1, ...) {
  UseMethod("slide_by")
}

slide_by.tbl_ts <- function(.data, .size = 1, .step = 1, ...) {
  key_data <- key_data(.data)
  lst_rows <- as_list_of(map(key_rows(.data),
    slider2, .size = .size, .step = .step))
  key_data[[".rows"]] <- lst_rows
  nfolds <- max(map_int(lst_rows, vec_size))
  new_rolling_tsibble(.data, key_data = key_data, .nfolds = nfolds)
}

slide_by.grouped_ts <- function(.data, .size = 1, .step = 1, ...) {
  warn("Ignoring grouping structure.")
  slide_by.tbl_ts(.data, .size, .step)
}

slide_by.tbl_rts <- function(.data, ...) {
  .data
}

collect.tbl_rts <- function(x, fold = integer(), ...) {
  if (is_empty(fold))  return(as_tsibble(x, validate = FALSE))
  if (!has_length(fold, 1)) {
    abort("`fold` only accepts integer of length 1.")
  }
  max_folds <- nfolds(x)
  if (fold > max_folds) {
    abort(sprintf("The maximum `fold` is %s.", max_folds))
  }
  out_rows <- vec_c(!!!map(key_rows(x), fold))
  as_tsibble(vec_slice(x, out_rows), validate = FALSE)
}

slider2 <- function(x, .size = 1, .step = 1) {
  if (!(.size > 0 && .step > 0 && is_integerish(.size) && is_integerish(.step))) {
    abort("`.size` & `.step` must be a positive integer.")
  }
  len_x <- vec_size(x)
  lst_idx <- seq.int(1L, len_x - .size + 1, by = .step)
  as_list_of(map(lst_idx, function(idx) x[idx:(idx + .size - 1)]))
}

tbl_sum.tbl_rts <- function(x) {
  n_folds <- brackets(big_mark(nfolds(x)))
  res <- NextMethod()
  c(res, "Folds" = n_folds)
}

new_rolling_tsibble <- function(.data, key_data, .nfolds = NULL) {
  attr(.data, "key") <- key_data
  new_tsibble(.data, .nfolds = .nfolds, class = "tbl_rts")
}

nfolds <- function(.data) {
  .data %@% .nfolds
}
