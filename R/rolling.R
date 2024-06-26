slider2 <- function(.x, .size = 1, .step = 1) {
  .x <- check_slider_input(.x, .size = .size, .step = .step)
  len_x <- NROW(.x)
  abs_size <- abs(.size)
  if (abs_size > len_x) {
    abort(sprintf(slider_msg(), abs_size, len_x))
  }
  sign <- sign(.size)
  if (sign > 0) {
    lst_idx <- seq.int(1L, len_x - abs_size + 1, by = .step)
  } else {
    lst_idx <- seq.int(len_x, abs_size, by = sign * .step)
  }
  map(lst_idx, function(idx) .x[idx:(idx + sign * (abs_size - 1))])
}

check_slider_input <- function(.x, .size = 1, .step = 1) {
  bad_window_function(.size)
  bad_step_function(.step)
  if (is.data.frame(.x)) .x <- as.list(.x)
  .x
}

#' Perform sliding windows on a tsibble by row
#'
#' `r lifecycle::badge('questioning')`
#'
#' @param .x A tsibble.
#' @param .size A positive integer for window size.
#' @param .step A positive integer for calculating at every specified step
#' instead of every single step.
#' @param .id A character naming the new column `.id` containing the partition.
#'
#' @section Rolling tsibble:
#' `slide_tsibble()`, `tile_tsibble()`, and `stretch_tsibble()` provide fast
#' and shorthand for rolling over a tsibble by observations. That said, if the
#' supplied tsibble has time gaps, these rolling helpers will ignore those gaps
#' and proceed.
#'
#' They are useful for preparing the tsibble for time series cross validation.
#' They all return a tsibble including a new column `.id` as part of the key. The
#' output dimension will increase considerably with `slide_tsibble()` and
#' `stretch_tsibble()`, which is likely to run out of memory when the data is
#' large.
#' @family rolling tsibble
#' @keywords internal
#' @export
#' @examples
#' harvest <- tsibble(
#'   year = rep(2010:2012, 2),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = fruit, index = year
#' )
#' harvest %>%
#'   slide_tsibble(.size = 2)
slide_tsibble <- function(.x, .size = 1, .step = 1, .id = ".id") {
  lst_indices <- map(key_rows(.x), slider2, .size = .size, .step = .step)
  roll_tsibble(.x, indices = lst_indices, .id = .id)
}

# fast_slider <- function(.x, .size = 1, .step = 1) {
#   len_x <- NROW(.x)
#   lst_idx <- seq.int(1L, len_x - .size + 1, by = .step)
#   len_idx <- length(lst_idx)
#   idx <- kronecker(lst_idx, matrix(seq_len(.size) - 1, ncol = .size), "+")
#   .x[idx, ]
#   # id_indices <- rep.int(seq_len(len_idx), .size)
#   # list(indices = .x[idx], id_indices = id_indices)
# }

roll_tsibble <- function(.x, indices, .id = ".id") {
  if (.id %in% names(.x)) {
    abort(sprintf("Can't overwrite existing column `%s`.", .id))
  }
  tbl <- as_tibble(ungroup(.x))
  row_indices <- unlist(indices, use.names = FALSE)
  id_indices <-
    unlist(map(
      indices,
      function(.x) imap(.x, function(.x, .y) rep.int(.y, length(.x)))
    ), use.names = FALSE)
  res <-
    group_by(
      mutate(tbl[row_indices, ], !!.id := id_indices),
      !!!groups(.x)
    )
  new_key <- c(.id, key_vars(.x))
  build_tsibble(res,
    key = !!new_key, index = !!index(.x), index2 = !!index2(.x),
    interval = interval(.x), validate = FALSE
  )
}

#' Perform tiling windows on a tsibble by row
#'
#' `r lifecycle::badge('questioning')`
#'
#' @param .x A tsibble.
#' @param .size A positive integer for window size.
#' @param .id A character naming the new column `.id` containing the partition.
#'
#' @inheritSection slide_tsibble Rolling tsibble
#' @family rolling tsibble
#' @keywords internal
#' @export
#' @examples
#' harvest <- tsibble(
#'   year = rep(2010:2012, 2),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = fruit, index = year
#' )
#' harvest %>%
#'   tile_tsibble(.size = 2)
tile_tsibble <- function(.x, .size = 1, .id = ".id") {
  lst_indices <- map(key_rows(.x), tiler2, .size = .size)
  roll_tsibble(.x, indices = lst_indices, .id = .id)
}

tiler2 <- function(.x, .size = 1) {
  bad_window_function(.size)
  len_x <- NROW(.x)
  seq_x <- seq_len(len_x)
  denom <- len_x + 1
  frac <- ceiling((seq_x %% denom) / .size)
  unname(split(.x, frac, drop = TRUE))
}

#' Perform stretching windows on a tsibble by row
#'
#' `r lifecycle::badge('questioning')`
#'
#' @param .x A tsibble.
#' @param .step A positive integer for incremental step.
#' @param .init A positive integer for an initial window size.
#' @param .id A character naming the new column `.id` containing the partition.
#'
#' @inheritSection slide_tsibble Rolling tsibble
#' @family rolling tsibble
#' @keywords internal
#' @export
#' @examples
#' harvest <- tsibble(
#'   year = rep(2010:2012, 2),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = fruit, index = year
#' )
#' harvest %>%
#'   stretch_tsibble()
stretch_tsibble <- function(.x, .step = 1, .init = 1, .id = ".id") {
  lst_indices <- map(key_rows(.x), stretcher2, .step = .step, .init = .init)
  roll_tsibble(.x, indices = lst_indices, .id = .id)
}

stretcher2 <- function(.x, .step = 1, .init = 1) {
  bad_window_function(.step)
  if (!is_integerish(.init, n = 1) || .init < 1) {
    abort("`.init` only accepts a positive integer.")
  }
  if (is.data.frame(.x)) .x <- as.list(.x)
  len_x <- NROW(.x)
  if (len_x == 1) {
    abort("`.x` of length one cannot be stretched.")
  }
  if (len_x <= .init) {
    abort(sprintf("`.init` must be less than the length of `.x` (%s).", len_x))
  }
  abs_size <- abs(.step)
  counter <- incr(.init = .init, .step = abs_size)
  if (sign(.step) < 0) .x <- rev(.x)
  ncall <- seq_len(floor((len_x - .init) / abs_size))
  incr_lst <- c(list(seq_len(.init)), map(ncall, function(z) seq_len(counter())))
  map(incr_lst, function(idx) .x[idx])
}

bad_window_function <- function(.size) {
  if (!is_integerish(.size, n = 1)) {
    abort("`.size` must be an integer.")
  }
  if (.size == 0) {
    abort("`.size` must not be 0.")
  }
}

bad_step_function <- function(.step) {
  if (.step <= 0 || !is_integerish(.step, n = 1)) {
    abort("`.step` must be a positive integer.")
  }
}

incr <- function(.init, .step) {
  .init
  function() {
    .init <<- .init + .step
    .init
  }
}

slider_msg <- function() {
  "`abs(.size)` (%s) must not be larger than the length (%s) of the input."
}
