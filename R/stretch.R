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
#'
#' @rdname stretch
#' @export
#' @seealso
#' * [stretch2], [pstretch]
#' * [slide] for sliding window with overlapping observations
#' * [tile] for tiling window without overlapping observations
#'
#' @examples
#' x <- 1:5
#' lst <- list(x = x, y = 6:10, z = 11:15)
#' stretch_dbl(x, mean, .size = 2)
#' stretch_lgl(x, ~ mean(.) > 2, .size = 2)
#' stretch(lst, ~ ., .size = 2)
stretch <- function(.x, .f, ..., .size = 1, .init = 1, .bind = FALSE) {
  lst_x <- stretcher(.x, .size = .size, .init = .init, .bind)
  purrr::map(lst_x, .f, ...)
}

#' @evalRd paste0('\\alias{stretch_', c("lgl", "chr", "dbl", "int"), '}')
#' @name stretch
#' @rdname stretch
#' @exportPattern ^stretch_
for(type in c("lgl", "chr", "dbl", "int")){
  assign(
    paste0("stretch_", type),
    replace_fn_names(stretch, list(map = sym(paste0("map_", type))))
  )
}

#' @rdname stretch
#' @export
stretch_dfr <- function(
  .x, .f, ..., .size = 1, .init = 1, .bind = FALSE, .id = NULL
) {
  out <- stretch(
    .x, .f = .f, ..., .size = .size, .init = .init, 
    .bind = .bind
  )
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch
#' @export
stretch_dfc <- function(.x, .f, ..., .size = 1, .init = 1, .bind = FALSE) {
  out <- stretch(
    .x, .f = .f, ..., .size = .size, .init = .init,
    .bind = .bind
  )
  dplyr::bind_cols(!!! out)
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
#' @param .size,.init An integer for moving and initial window size.
#'
#' @rdname stretch2
#' @export
#' @seealso
#' * [stretch]
#' * [slide2] for sliding window with overlapping observations
#' * [tile2] for tiling window without overlapping observations
#'
#' @examples
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' lst <- list(x = x, y = y, z = z)
#' df <- as.data.frame(lst)
#' stretch2(x, y, sum, .size = 2)
#' stretch2(lst, lst, ~ ., .size = 2)
#' stretch2(df, df, ~ ., .size = 2)
#' pstretch(lst, sum, .size = 1)
#' pstretch(list(lst, lst), ~ ., .size = 2)
stretch2 <- function(.x, .y, .f, ..., .size = 1, .init = 1, .bind = FALSE) {
  lst <- pstretcher(.x, .y, .size = .size, .init = .init, .bind = .bind)
  purrr::map2(lst[[1]], lst[[2]], .f, ...)
}

#' @evalRd paste0('\\alias{stretch2_', c("lgl", "chr", "dbl", "int"), '}')
#' @name stretch2
#' @rdname stretch2
#' @exportPattern ^stretch2_
for(type in c("lgl", "chr", "dbl", "int")){
  assign(
    paste0("stretch2_", type),
    replace_fn_names(stretch2, list(map2 = sym(paste0("map2_", type))))
  )
}

#' @rdname stretch2
#' @export
stretch2_dfr <- function(
  .x, .y, .f, ..., .size = 1, .init = 1, .bind = FALSE, .id = NULL
) {
  out <- stretch2(
    .x, .y, .f = .f, ..., .size = .size, .init = .init,
    .bind = .bind
  )
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch2
#' @export
stretch2_dfc <- function(
  .x, .y, .f, ..., .size = 1, .init = 1, .bind = FALSE
) {
  out <- stretch2(
    .x, .y, .f = .f, ..., .size = .size, .init = .init,
    .bind = .bind
  )
  dplyr::bind_cols(!!! out)
}

#' @rdname stretch2
#' @export
pstretch <- function(.l, .f, ..., .size = 1, .init = 1, .bind = FALSE) {
  lst <- pstretcher(!!! .l, .size = .size, .init = .init, .bind = .bind)
  purrr::pmap(lst, .f, ...)
}

#' @evalRd paste0('\\alias{pstretch_', c("lgl", "chr", "dbl", "int"), '}')
#' @name pstretch
#' @rdname stretch2
#' @exportPattern ^pstretch_
for(type in c("lgl", "chr", "dbl", "int")){
  assign(
    paste0("pstretch_", type),
    replace_fn_names(pstretch, list(pmap = sym(paste0("pmap_", type))))
  )
}

#' @rdname stretch2
#' @export
pstretch_dfr <- function(
  .l, .f, ..., .size = 1, .init = 1, .bind = FALSE, .id = NULL
) {
  out <- pstretch(.l, .f, ..., .size = .size, .init = .init, .bind = .bind)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch2
#' @export
pstretch_dfc <- function(.l, .f, ..., .size = 1, .init = 1, .bind = FALSE) {
  out <- pstretch(.l, .f, ..., .size = .size, .init = .init, .bind = .bind)
  dplyr::bind_cols(!!! out)
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
#' stretcher(x, .size = 2)
#' stretcher(lst, .size = 2)
#' stretcher(df, .size = 2)
#' pstretcher(df, df, .size = 2)
stretcher <- function(.x, .size = 1, .init = 1, .bind = FALSE) {
  bad_window_function(.size)
  if (!is_integerish(.init, n = 1) || .init < 1) {
    abort("`.init` must be a positive integer.")
  }
  if (is.data.frame(.x)) .x <- as.list(.x)
  len_x <- NROW(.x)
  abs_size <- abs(.size)
  counter <- incr(init = .init, size = abs_size)
  if (sign(.size) < 0) .x <- rev(.x)
  ncall <- seq_len(ceiling((len_x - .init) / abs_size) - 1)
  incr_lst <- c(
    list(seq_len(.init)),
    purrr::map(ncall, ~ seq_len(counter())),
    list(seq_len(len_x))
  )
  out <- purrr::map(incr_lst, function(idx) .x[idx])
  if (.bind) bind_lst(out) else out
}


#' @rdname stretcher
#' @export
pstretcher <- function(..., .size = 1, .init = 1, .bind = FALSE) { # parallel sliding
  lst <- recycle(list2(...))
  purrr::map(lst, function(x) stretcher(x, .size, .init, .bind))
}

incr <- function(init, size) {
  init
  function() {
    init <<- init + size
    init
  }
}
