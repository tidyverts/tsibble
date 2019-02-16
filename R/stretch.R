abort_stretch_size <- function(...) {
  dots <- dots_list(...)
  if (".size" %in% names(dots)) {
    abort("Argument `.size` is retired. Please use `.step`.")
  }
}

#' Stretching window calculation
#'
#' Fixing an initial window and expanding more observations:
#' * `stretch()` always returns a list.
#' * `stretch_lgl()`, `stretch_int()`, `stretch_dbl()`, `stretch_chr()` use the same
#' arguments as `stretch()`, but return vectors of the corresponding type.
#' * `stretch_dfr()` `stretch_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide
#' @param .init A positive integer for an initial window size.
#' @param .step A positive integer for incremental step.
#'
#' @rdname stretch
#' @export
#' @family stretching window functions
#' @seealso
#' * [future_stretch] for stretching window in parallel
#' * [slide] for sliding window with overlapping observations
#' * [tile] for tiling window without overlapping observations
#'
#' @examples
#' x <- 1:5
#' stretch_dbl(x, mean, .step = 2)
#' stretch_lgl(x, ~ mean(.) > 2, .step = 2)
#' lst <- list(x = x, y = 6:10, z = 11:15)
#' stretch(lst, ~ ., .step = 2, .fill = NULL)
stretch <- function(.x, .f, ..., .step = 1, .init = 1, .fill = NA,
  .bind = FALSE) {
  abort_stretch_size(...)
  lst_x <- stretcher(.x, .step = .step, .init = .init, .bind = .bind)
  out <- map(lst_x, .f, ...)
  pad_stretch(out, .init = .init, .step = .step, .fill = .fill)
}

#' @evalRd paste0('\\alias{stretch_', c("lgl", "chr", "dbl", "int"), '}')
#' @name stretch
#' @rdname stretch
#' @exportPattern ^stretch_
for (type in c("lgl", "chr", "dbl", "int")) {
  assign(
    paste0("stretch_", type),
    replace_fn_names(stretch, list(map = paste0("map_", type)))
  )
}

#' @rdname stretch
#' @export
stretch_dfr <- function(
  .x, .f, ..., .step = 1, .init = 1, .fill = NA, .bind = FALSE, .id = NULL
) {
  out <- stretch(
    .x, .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, .id = .id)
}

#' @rdname stretch
#' @export
stretch_dfc <- function(.x, .f, ..., .step = 1, .init = 1, .fill = NA,
  .bind = FALSE) {
  out <- stretch(
    .x, .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, byrow = FALSE)
}

#' Stretching window calculation over multiple simultaneously
#'
#' Fixing an initial window and expanding more observations:
#' * `stretch2()` and `pstretch()` always returns a list.
#' * `stretch2_lgl()`, `stretch2_int()`, `stretch2_dbl()`, `stretch2_chr()` use the same
#' arguments as `stretch2()`, but return vectors of the corresponding type.
#' * `stretch2_dfr()` `stretch2_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide2
#' @inheritParams stretch
#'
#' @rdname stretch2
#' @export
#' @family stretching window functions
#' @seealso
#' * [slide2] for sliding window with overlapping observations
#' * [tile2] for tiling window without overlapping observations
#'
#' @examples
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' lst <- list(x = x, y = y, z = z)
#' df <- as.data.frame(lst)
#' stretch2(x, y, sum, .step = 2)
#' stretch2(lst, lst, ~ ., .step = 2)
#' stretch2(df, df, ~ ., .step = 2)
#' pstretch(lst, sum, .step = 1)
#' pstretch(list(lst, lst), ~ ., .step = 2)
#'
#' ###
#' # row-wise stretching over data frame
#' ###
#'
#' x <- as.Date("2017-01-01") + 0:364
#' df <- data.frame(x = x, y = seq_along(x))
#'
#' tibble(
#'   data = pstretch(df, function(...) as_tibble(list(...)), .init = 10)
#' )
stretch2 <- function(.x, .y, .f, ..., .step = 1, .init = 1, .fill = NA,
  .bind = FALSE) {
  abort_stretch_size(...)
  lst <- pstretcher(.x, .y, .step = .step, .init = .init, .bind = .bind)
  out <- map2(lst[[1]], lst[[2]], .f, ...)
  pad_stretch(out, .init = .init, .step = .step, .fill = .fill)
}

#' @evalRd paste0('\\alias{stretch2_', c("lgl", "chr", "dbl", "int"), '}')
#' @name stretch2
#' @rdname stretch2
#' @exportPattern ^stretch2_
for (type in c("lgl", "chr", "dbl", "int")) {
  assign(
    paste0("stretch2_", type),
    replace_fn_names(stretch2, list(map2 = paste0("map2_", type)))
  )
}

#' @rdname stretch2
#' @export
stretch2_dfr <- function(
  .x, .y, .f, ..., .step = 1, .init = 1, .fill = NA, .bind = FALSE, .id = NULL
) {
  out <- stretch2(
    .x, .y, .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, .id = .id)
}

#' @rdname stretch2
#' @export
stretch2_dfc <- function(
  .x, .y, .f, ..., .step = 1, .init = 1, .fill = NA, .bind = FALSE
) {
  out <- stretch2(
    .x, .y, .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, byrow = FALSE)
}

#' @rdname stretch2
#' @export
pstretch <- function(.l, .f, ..., .step = 1, .init = 1, .fill = NA,
  .bind = FALSE) {
  abort_stretch_size(...)
  lst <- pstretcher(!!! .l, .step = .step, .init = .init,
    .bind = .bind)
  out <- pmap(lst, .f, ...)
  pad_stretch(out, .init = .init, .step = .step, .fill = .fill)
}

#' @evalRd paste0('\\alias{pstretch_', c("lgl", "chr", "dbl", "int"), '}')
#' @name pstretch
#' @rdname stretch2
#' @exportPattern ^pstretch_
for (type in c("lgl", "chr", "dbl", "int")) {
  assign(
    paste0("pstretch_", type),
    replace_fn_names(pstretch, list(pmap = paste0("pmap_", type)))
  )
}

#' @rdname stretch2
#' @export
pstretch_dfr <- function(
  .l, .f, ..., .step = 1, .init = 1, .fill = NA, .bind = FALSE, .id = NULL
) {
  out <- pstretch(.l, .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind)
  bind_df(out, .size = .init, .fill = .fill, .id = .id)
}

#' @rdname stretch2
#' @export
pstretch_dfc <- function(.l, .f, ..., .step = 1, .init = 1, .fill = NA,
  .bind = FALSE) {
  out <- pstretch(.l, .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind)
  bind_df(out, .size = .init, .fill = .fill, byrow = FALSE)
}

#' Split the input to a list according to the stretching window size.
#'
#' @param .x An objects to be split.
#' @param ... Multiple objects to be split in parallel.
#' @inheritParams stretch
#' @rdname stretcher
#' @export
#' @examples
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' lst <- list(x = x, y = y, z = z)
#' df <- as.data.frame(lst)
#'
#' stretcher(x, .step = 2)
#' stretcher(lst, .step = 2)
#' stretcher(df, .step = 2)
#' pstretcher(df, df, .step = 2)
stretcher <- function(.x, .step = 1, .init = 1, .bind = FALSE) {
  bad_window_function(.step)
  if (!is_integerish(.init, n = 1) || .init < 1) {
    abort("`.init` must be a positive integer.")
  }
  abort_not_lst(.x, .bind = .bind)
  if (is.data.frame(.x)) .x <- as.list(.x)
  len_x <- NROW(.x)
  if (len_x <= .init) {
    abort(sprintf("`.init` must be less than %s.", len_x))
  }
  abs_size <- abs(.step)
  counter <- incr(.init = .init, .step = abs_size)
  if (sign(.step) < 0) .x <- rev(.x)
  ncall <- seq_len(floor((len_x - .init) / abs_size))
  incr_lst <- c(list(seq_len(.init)), map(ncall, ~ seq_len(counter())))
  # incr_lst <- c(
  #   list(seq_len(.init)),
  #   map(ncall, ~ seq_len(counter())),
  #   list(seq_len(len_x))
  # )
  out <- map(incr_lst, function(idx) .x[idx])
  if (.bind) bind_lst(out) else out
}


#' @rdname stretcher
#' @export
pstretcher <- function(..., .step = 1, .init = 1, .bind = FALSE) { # parallel sliding
  abort_stretch_size(...)
  lst <- recycle(list2(...))
  map(lst, function(x) stretcher(x, .step, .init, .bind))
}

#' Perform stretching windows on a tsibble by row
#'
#' @param .x A tsibble.
#' @param .step A positive integer for incremental step.
#' @inheritParams stretch
#' @param .id A character naming the new column `.id` containing the partition.
#'
#' @return A tsibble
#' @family rolling tsibble
#' @export
#' @examples
#' harvest <- tsibble(
#'   year = rep(2010:2012, 2),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = id(fruit), index = year
#' )
#' harvest %>%
#'   stretch_tsibble()
stretch_tsibble <- function(.x, .step = 1, .init = 1, .id = ".id") {
  lst_indices <- map(key_rows(.x), stretcher, .step = .step, .init = .init)
  roll_tsibble(.x, indices = lst_indices, .id = .id)
}

incr <- function(.init, .step) {
  .init
  function() {
    .init <<- .init + .step
    .init
  }
}

pad_stretch <- function(x, .init = 1, .step = 1, .fill = NA) {
  if (is_null(.fill)) return(x)

  len_x <- length(x)
  fill_size <- abs(.init - .step)
  if (.step == 1) {
    c(rep(.fill, fill_size), x)
  } else {
    seq_x <- seq_len(len_x)
    rep_idx <- rep.int(len_x + 1, len_x * fill_size)
    null_idx <- matrix(rep_idx, nrow = fill_size)
    idx <- as.integer(rbind(seq_x, null_idx, deparse.level = 0))
    idx <- idx[-((length(idx) - fill_size + 1):length(idx))]
    res <- x[idx]
    res[!(idx %in% seq_x)] <- .fill
    if (.init > 1) {
      c(rep(.fill, .init - 1), res)
    } else {
      res
    }
  }
}

#' Stretching window in parallel
#'
#' Multiprocessing equivalents of [slide()], [tile()], [stretch()] prefixed by `future_`.
#' * Variants for corresponding types: `future_*_lgl()`, `future_*_int()`,
#' `future_*_dbl()`, `future_*_chr()`, `future_*_dfr()`, `future_*_dfc()`.
#' * Extra arguments `.progress` and `.options` for enabling progress bar and the
#' future specific options to use with the workers.
#'
#' @evalRd {suffix <- c("lgl", "chr", "int", "dbl", "dfr", "dfc"); c(paste0('\\alias{future_', c("stretch", "stretch2", "pstretch"), '}'), paste0('\\alias{future_stretch_', suffix, '}'), paste0('\\alias{future_stretch2_', suffix, '}'), paste0('\\alias{future_pstretch_', suffix, '}'))}
#' @name future_stretch()
#' @rdname future-stretch
#' @exportPattern ^future_
# nocov start
assign("future_stretch", replace_fn_names(stretch, list(map = "future_map"), ns = "furrr"))
assign("future_stretch2", replace_fn_names(stretch2, list(map2 = "future_map2"), ns = "furrr"))
assign("future_pstretch", replace_fn_names(pstretch, list(pmap = "future_pmap"), ns = "furrr"))
assign("future_stretch_dfr", replace_fn_names(stretch_dfr, list(stretch = "future_stretch")))
assign("future_stretch2_dfr", replace_fn_names(stretch2_dfr, list(stretch2 = "future_stretch2")))
assign("future_pstretch_dfr", replace_fn_names(pstretch_dfr, list(pstretch = "future_pstretch")))
assign("future_stretch_dfc", replace_fn_names(stretch_dfc, list(stretch = "future_stretch")))
assign("future_stretch2_dfc", replace_fn_names(stretch2_dfc, list(stretch2 = "future_stretch2")))
assign("future_pstretch_dfc", replace_fn_names(pstretch_dfc, list(pstretch = "future_pstretch")))
for (type in c("lgl", "chr", "int", "dbl")) {
  assign(
    paste0("future_stretch_", type),
    replace_fn_names(stretch, list(map = paste0("future_map_", type)), ns = "furrr")
  )
  assign(
    paste0("future_stretch2_", type),
    replace_fn_names(stretch2, list(map2 = paste0("future_map2_", type)), ns = "furrr")
  )
  assign(
    paste0("future_pstretch_", type),
    replace_fn_names(pstretch, list(pmap = paste0("future_pmap_", type)), ns = "furrr")
  )
}
# nocov end
