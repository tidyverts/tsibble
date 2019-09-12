slide_by <- function(data, size = 1, step = 1) {
  if (is_false(
    size > 0 && step > 0 && 
    is_integerish(size, n = 1) && is_integerish(step, n = 1)
  )) {
    abort("`size` & `step` must be a positive integer.")
  }
  UseMethod("slide_by")
}

slide_by.tbl_ts <- function(data, size = 1, step = 1) {
  key_rows <- key_rows(data)
  nfolds <- max(map_dbl(key_rows, function(x) slider_nfolds(x, size, step)))
  new_rolling_tsibble(data, 
    .nfolds = nfolds,
    .roller = function(x, fold) slider_anon(x, size, step, fold)
  )
}

slide_by.grouped_ts <- function(data, size = 1, step = 1) {
  warn("Ignoring grouping structure.")
  slide_by.tbl_ts(data, size, step)
}

slider_nfolds <- function(x, size = 1, step = 1) {
  ceiling((vec_size(x) - size + 1) / step)
}

slider_anon <- function(x, size = 1, step = 1, fold) {
  len_x <- vec_size(x)
  start_x <- 1 + (fold - 1) * step
  end_x <- start_x + size - 1
  max_x <- if (end_x > len_x) len_x else end_x
  vec_slice(x, start_x:max_x)
}

stretch_by <- function(data, init = 1, step = 1) {
  if (is_false(
    init > 0 && step > 0 && 
    is_integerish(init, n = 1) && is_integerish(step, n = 1)
  )) {
    abort("`init` & `step` must be a positive integer.")
  }
  len_x <- vec_size(data)
  if (len_x <= init) {
    abort(sprintf("`init` must be less than %s.", len_x))
  }
  UseMethod("stretch_by")
}

stretch_by.tbl_ts <- function(data, init = 1, step = 1) {
  key_rows <- key_rows(data)
  nfolds <- max(map_dbl(key_rows, function(x) stretcher_nfolds(x, init, step)))
  new_rolling_tsibble(data, 
    .nfolds = nfolds,
    .roller = function(x, fold) stretcher_anon(x, init, step, fold)
  )
}

stretch_by.grouped_ts <- function(data, init = 1, step = 1) {
  warn("Ignoring grouping structure.")
  stretch_by.tbl_ts(data, init, step)
}

stretcher_nfolds <- function(x, init = 1, step = 1) {
  floor((vec_size(x) - init) / step) + 1
}

stretcher_anon <- function(x, init = 1, step = 1, fold) {
  len_x <- vec_size(x)
  end_x <- init + (fold - 1) * step
  max_x <- if (end_x > len_x) len_x else end_x
  vec_slice(x, seq_len(max_x))
}

collect.tbl_rts <- function(x, fold = integer()) {
  if (is_empty(fold))  return(as_tsibble(x))
  if (!has_length(fold, 1)) {
    abort("`fold` only accepts integer of length 1.")
  }
  max_folds <- nfolds(x)
  if (fold > max_folds) {
    abort(sprintf("The maximum `fold` is %s.", max_folds))
  }
  out_rows <- vec_c(!!!map(key_rows(x), function(z) roller(x)(z, fold = fold)))
  # as_tsibble(vec_slice(x, out_rows))
  x[out_rows, ]
}

tbl_sum.tbl_rts <- function(x) {
  c(NextMethod(), "Folds" = brackets(big_mark(nfolds(x))))
}

as_tsibble.tbl_rts <- function(x) {
  new_tsibble(x, ".nfolds" = NULL, ".roller" = NULL)
}

new_rolling_tsibble <- function(data, .nfolds = NULL, .roller = NULL) {
  new_tsibble(data, .nfolds = .nfolds, .roller = .roller, class = "tbl_rts")
}

nfolds <- function(data) {
  data %@% ".nfolds"
}

roller <- function(data) {
  data %@% ".roller"
}
