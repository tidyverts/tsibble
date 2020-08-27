#' Tiling window calculation
#'
#' @description
#' \lifecycle{defunct}
#'
#' Please consider using the [slider](https://davisvaughan.github.io/slider) package.
#'
#' Tiling window without overlapping observations:
#' * `tile()` always returns a list.
#' * `tile_lgl()`, `tile_int()`, `tile_dbl()`, `tile_chr()` use the same
#' arguments as `tile()`, but return vectors of the corresponding type.
#' * `tile_dfr()` `tile_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide
#'
#' @keywords internal
#' @rdname tile
#' @export
#' @family tiling window functions
#' @seealso
#' * [slide] for sliding window with overlapping observations
#' * [stretch] for expanding more observations
#'
tile <- function(.x, .f, ..., .size = 1, .bind = FALSE) {
  lst_x <- tiler(.x, .size = .size, .bind = .bind)
  map(lst_x, .f, ...)
}

#' @evalRd paste0('\\alias{tile_', c("lgl", "chr", "dbl", "int"), '}')
#' @name tile
#' @rdname tile
#' @exportPattern ^tile_
for (type in c("lgl", "chr", "dbl", "int")) {
  assign(
    paste0("tile_", type),
    replace_fn_names(tile, list(map = paste0("map_", type)))
  )
}

#' @rdname tile
#' @export
tile_dfr <- function(.x, .f, ..., .size = 1, .bind = FALSE, .id = NULL) {
  out <- tile(.x = .x, .f = .f, ..., .size = .size, .bind = .bind)
  bind_rows(!!!out, .id = .id)
}

#' @rdname tile
#' @export
tile_dfc <- function(.x, .f, ..., .size = 1, .bind = FALSE) {
  out <- tile(.x = .x, .f = .f, ..., .size = .size, .bind = .bind)
  bind_cols(!!!out)
}

#' Tiling window calculation over multiple inputs simultaneously
#'
#' @description
#' \lifecycle{defunct}
#'
#' Please consider using the [slider](https://davisvaughan.github.io/slider) package.
#'
#' Tiling window without overlapping observations:
#' * `tile2()` and `ptile()` always returns a list.
#' * `tile2_lgl()`, `tile2_int()`, `tile2_dbl()`, `tile2_chr()` use the same
#' arguments as `tile2()`, but return vectors of the corresponding type.
#' * `tile2_dfr()` `tile2_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide2
#' @keywords internal
#' @rdname tile2
#' @export
#' @family tiling window functions
tile2 <- function(.x, .y, .f, ..., .size = 1, .bind = FALSE) {
  lst <- ptiler(.x, .y, .size = .size, .bind = .bind)
  map2(lst[[1]], lst[[2]], .f, ...)
}

#' @evalRd paste0('\\alias{tile2_', c("lgl", "chr", "dbl", "int"), '}', collapse = '\n')
#' @name tile2
#' @rdname tile2
#' @exportPattern ^tile2_
for (type in c("lgl", "chr", "dbl", "int")) {
  assign(
    paste0("tile2_", type),
    replace_fn_names(tile2, list(map2 = paste0("map2_", type)))
  )
}

#' @rdname tile2
#' @export
tile2_dfr <- function(.x, .y, .f, ..., .size = 1, .bind = FALSE, .id = NULL) {
  out <- tile2(.x, .y, .f = .f, ..., .size = .size, .bind = .bind)
  bind_rows(!!!out, .id = .id)
}

#' @rdname tile2
#' @export
tile2_dfc <- function(.x, .y, .f, ..., .size = 1, .bind = FALSE) {
  out <- tile2(.x, .y, .f = .f, ..., .size = .size, .bind = .bind)
  bind_cols(!!!out)
}

#' @rdname tile2
#' @export
ptile <- function(.l, .f, ..., .size = 1, .bind = FALSE) {
  lst <- ptiler(!!!.l, .size = .size, .bind = .bind)
  pmap(lst, .f, ...)
}

#' @evalRd paste0('\\alias{ptile_', c("lgl", "chr", "dbl", "int"), '}')
#' @name ptile
#' @rdname tile2
#' @exportPattern ^ptile_
for (type in c("lgl", "chr", "dbl", "int")) {
  assign(
    paste0("ptile_", type),
    replace_fn_names(ptile, list(pmap = paste0("pmap_", type)))
  )
}

#' @rdname tile2
#' @export
ptile_dfr <- function(.l, .f, ..., .size = 1, .bind = FALSE, .id = NULL) {
  out <- ptile(.l, .f, ..., .size = .size, .bind = .bind)
  bind_rows(!!!out, .id = .id)
}

#' @rdname tile2
#' @export
ptile_dfc <- function(.l, .f, ..., .size = 1, .bind = FALSE) {
  out <- ptile(.l, .f, ..., .size = .size, .bind = .bind)
  bind_cols(!!!out)
}

#' Splits the input to a list according to the tiling window size.
#'
#' @inheritParams slider
#' @keywords internal
#' @rdname tiler
#' @export
tiler <- function(.x, .size = 1, .bind = FALSE) {
  lifecycle::deprecate_stop("0.9.0", "tile()", "slider::slide()")
  tiler2(.x, .size, .bind)
}

tiler2 <- function(.x, .size = 1, .bind = FALSE) {
  bad_window_function(.size)
  abort_not_lst(.x, .bind = .bind)
  len_x <- NROW(.x)
  seq_x <- seq_len(len_x)
  denom <- len_x + 1
  frac <- ceiling((seq_x %% denom) / .size)
  out <- unname(split(.x, frac, drop = TRUE))
  if (.bind) bind_lst(out) else out
}

#' @rdname tiler
#' @export
ptiler <- function(..., .size = 1, .bind = FALSE) { # parallel tiling
  lst <- recycle(list2(...))
  map(lst, function(x) tiler(x, .size = .size, .bind = .bind))
}

#' Perform tiling windows on a tsibble by row
#'
#' \lifecycle{questioning}
#'
#' @param .x A tsibble.
#' @param .size A positive integer for window size.
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
#'   tile_tsibble(.size = 2)
tile_tsibble <- function(.x, .size = 1, .id = ".id") {
  lst_indices <- map(key_rows(.x), tiler2, .size = .size)
  roll_tsibble(.x, indices = lst_indices, .id = .id)
}
