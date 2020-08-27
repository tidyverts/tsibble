nrow2 <- function(.x) {
  if (is.data.frame(.x)) .x <- as.list(.x)
  NROW(.x)
}

#' Stretching window calculation
#'
#' @description
#' \lifecycle{defunct}
#'
#' Please consider using the [slider](https://davisvaughan.github.io/slider) package.
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
#' @return if `.fill != NULL`, it always returns the same length as input.
#' @keywords internal
#' @rdname stretch
#' @export
#' @family stretching window functions
stretch <- function(.x, .f, ..., .step = 1, .init = 1, .fill = NA,
                    .bind = FALSE) {
  lst_x <- stretcher(.x, .step = .step, .init = .init, .bind = .bind)
  out <- map(lst_x, .f, ...)
  pad_stretch(out,
    .init = .init, .step = .step, .fill = .fill,
    expect_length = nrow2(.x)
  )
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
stretch_dfr <- function(.x, .f, ..., .step = 1, .init = 1, .fill = NA,
                        .bind = FALSE, .id = NULL) {
  out <- stretch(
    .x,
    .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, .id = .id)
}

#' @rdname stretch
#' @export
stretch_dfc <- function(.x, .f, ..., .step = 1, .init = 1, .fill = NA,
                        .bind = FALSE) {
  out <- stretch(
    .x,
    .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, byrow = FALSE)
}

#' Stretching window calculation over multiple simultaneously
#'
#' @description
#' \lifecycle{defunct}
#'
#' Please consider using the [slider](https://davisvaughan.github.io/slider) package.
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
#' @keywords internal
#' @rdname stretch2
#' @export
#' @family stretching window functions
#' @seealso
#' * [slide2] for sliding window with overlapping observations
#' * [tile2] for tiling window without overlapping observations
#'
stretch2 <- function(.x, .y, .f, ..., .step = 1, .init = 1, .fill = NA,
                     .bind = FALSE) {
  lst <- pstretcher(.x, .y, .step = .step, .init = .init, .bind = .bind)
  out <- map2(lst[[1]], lst[[2]], .f, ...)
  pad_stretch(out,
    .init = .init, .step = .step, .fill = .fill,
    expect_length = nrow2(recycle(list(.x, .y))[[1]])
  )
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
stretch2_dfr <- function(.x, .y, .f, ..., .step = 1, .init = 1, .fill = NA,
                         .bind = FALSE, .id = NULL) {
  out <- stretch2(
    .x, .y,
    .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, .id = .id)
}

#' @rdname stretch2
#' @export
stretch2_dfc <- function(.x, .y, .f, ..., .step = 1, .init = 1, .fill = NA,
                         .bind = FALSE) {
  out <- stretch2(
    .x, .y,
    .f = .f, ..., .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, byrow = FALSE)
}

#' @rdname stretch2
#' @export
pstretch <- function(.l, .f, ..., .step = 1, .init = 1, .fill = NA,
                     .bind = FALSE) {
  lst <- pstretcher(!!!.l,
    .step = .step, .init = .init,
    .bind = .bind
  )
  out <- pmap(lst, .f, ...)
  pad_stretch(out,
    .init = .init, .step = .step, .fill = .fill,
    expect_length = nrow2(recycle(.l)[[1]])
  )
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
pstretch_dfr <- function(.l, .f, ..., .step = 1, .init = 1, .fill = NA,
                         .bind = FALSE, .id = NULL) {
  out <- pstretch(.l, .f, ...,
    .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, .id = .id)
}

#' @rdname stretch2
#' @export
pstretch_dfc <- function(.l, .f, ..., .step = 1, .init = 1, .fill = NA,
                         .bind = FALSE) {
  out <- pstretch(.l, .f, ...,
    .step = .step, .init = .init, .fill = .fill,
    .bind = .bind
  )
  bind_df(out, .size = .init, .fill = .fill, byrow = FALSE)
}

#' Split the input to a list according to the stretching window size.
#'
#' @param .x An objects to be split.
#' @param ... Multiple objects to be split in parallel.
#' @inheritParams stretch
#' @keywords internal
#' @rdname stretcher
#' @export
stretcher <- function(.x, .step = 1, .init = 1, .bind = FALSE) {
  lifecycle::deprecate_stop("0.9.0", "stretch()", "slider::slide()")
  stretcher2(.x, .step, .init, .bind)
}

stretcher2 <- function(.x, .step = 1, .init = 1, .bind = FALSE) {
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
  incr_lst <- c(list(seq_len(.init)), map(ncall, ~ seq_len(counter())))
  out <- map(incr_lst, function(idx) .x[idx])
  if (.bind) bind_lst(out) else out
}


#' @rdname stretcher
#' @export
pstretcher <- function(..., .step = 1, .init = 1, .bind = FALSE) { # parallel sliding
  lst <- recycle(list2(...))
  map(lst, function(x) stretcher(x, .step, .init, .bind))
}

#' Perform stretching windows on a tsibble by row
#'
#' \lifecycle{questioning}
#'
#' @param .x A tsibble.
#' @param .step A positive integer for incremental step.
#' @inheritParams stretch
#' @param .id A character naming the new column `.id` containing the partition.
#'
#' @inheritSection slide_tsibble Rolling tsibble
#' @family rolling tsibble
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

pad_stretch <- function(x, .init = 1, .step = 1, .fill = NA,
                        expect_length = NULL) {
  if (is_null(.fill)) {
    return(x)
  }

  len_x <- length(x)
  fill_size <- abs(.init - .step)
  if (.step == 1) {
    c(rep(.fill, fill_size), x)
  } else {
    seq_x <- seq_len(len_x)
    rep_idx <- rep.int(len_x + 1, len_x * (.step - 1))
    null_idx <- matrix(rep_idx, nrow = .step - 1)
    idx <- as.integer(rbind(seq_x, null_idx, deparse.level = 0))
    res <- x[idx]
    res[!(idx %in% seq_x)] <- .fill
    if (.init > 1) {
      res <- c(rep(.fill, .init - 1), res)
    }
    res[seq_len(expect_length)]
  }
}

incr <- function(.init, .step) {
  .init
  function() {
    .init <<- .init + .step
    .init
  }
}
