#' Tiling window calculation
#'
#' @description
#' \lifecycle{questioning}
#'
#' **The rolling window family will be deprecated in the future. Please consider
#' using the [slider](https://davisvaughan.github.io/slider) package.**
#'
#' Tiling window without overlapping observations:
#' * `tile()` always returns a list.
#' * `tile_lgl()`, `tile_int()`, `tile_dbl()`, `tile_chr()` use the same
#' arguments as `tile()`, but return vectors of the corresponding type.
#' * `tile_dfr()` `tile_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide
#'
#' @rdname tile
#' @export
#' @family tiling window functions
#' @seealso
#' * [future_tile] for tiling window in parallel
#' * [slide] for sliding window with overlapping observations
#' * [stretch] for expanding more observations
#'
#' @examples
#' x <- 1:5
#' lst <- list(x = x, y = 6:10, z = 11:15)
#' tile_dbl(x, mean, .size = 2)
#' tile_lgl(x, ~ mean(.) > 2, .size = 2)
#' tile(lst, ~., .size = 2)
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
#' \lifecycle{questioning}
#'
#' **The rolling window family will be deprecated in the future. Please consider
#' using the [slider](https://davisvaughan.github.io/slider) package.**
#'
#' Tiling window without overlapping observations:
#' * `tile2()` and `ptile()` always returns a list.
#' * `tile2_lgl()`, `tile2_int()`, `tile2_dbl()`, `tile2_chr()` use the same
#' arguments as `tile2()`, but return vectors of the corresponding type.
#' * `tile2_dfr()` `tile2_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide2
#' @rdname tile2
#' @export
#' @family tiling window functions
#' @seealso
#' * [slide2] for sliding window with overlapping observations
#' * [stretch2] for expanding more observations
#' @examples
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' lst <- list(x = x, y = y, z = z)
#' df <- as.data.frame(lst)
#' tile2(x, y, sum, .size = 2)
#' tile2(lst, lst, ~., .size = 2)
#' tile2(df, df, ~., .size = 2)
#' ptile(lst, sum, .size = 1)
#' ptile(list(lst, lst), ~., .size = 2)
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
#' @rdname tiler
#' @export
#' @examples
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' lst <- list(x = x, y = y, z = z)
#' df <- as.data.frame(lst)
#'
#' tiler(x, .size = 2)
#' tiler(lst, .size = 2)
#' ptiler(lst, .size = 2)
#' ptiler(list(x, y), list(y))
#' ptiler(df, .size = 2)
#' ptiler(df, df, .size = 2)
tiler <- function(.x, .size = 1, .bind = FALSE) {
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
#' @inheritParams tile
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
  lst_indices <- map(key_rows(.x), tiler, .size = .size)
  roll_tsibble(.x, indices = lst_indices, .id = .id)
}

#' Tiling window in parallel
#'
#' @description
#' \lifecycle{questioning}
#'
#' **The rolling window family will be deprecated in the future. Please consider
#' using the [slider](https://davisvaughan.github.io/slider) package.**
#' Multiprocessing equivalents of [slide()], [tile()], [stretch()] prefixed by `future_`.
#' * Variants for corresponding types: `future_*_lgl()`, `future_*_int()`,
#' `future_*_dbl()`, `future_*_chr()`, `future_*_dfr()`, `future_*_dfc()`.
#' * Extra arguments `.progress` and `.options` for enabling progress bar and the
#' future specific options to use with the workers.
#'
#' @evalRd {suffix <- c("lgl", "chr", "int", "dbl", "dfr", "dfc"); c(paste0('\\alias{future_', c("tile", "tile2", "ptile"), '}'), paste0('\\alias{future_tile_', suffix, '}'), paste0('\\alias{future_tile2_', suffix, '}'), paste0('\\alias{future_ptile_', suffix, '}'))}
#' @name future_tile()
#' @rdname future-tile
#' @exportPattern ^future_
# nocov start
assign("future_tile", replace_fn_names(tile, list(map = "future_map"), ns = "furrr"))
assign("future_tile2", replace_fn_names(tile2, list(map2 = "future_map2"), ns = "furrr"))
assign("future_ptile", replace_fn_names(ptile, list(pmap = "future_pmap"), ns = "furrr"))
assign("future_tile_dfr", replace_fn_names(tile_dfr, list(tile = "future_tile")))
assign("future_tile2_dfr", replace_fn_names(tile2_dfr, list(tile2 = "future_tile2")))
assign("future_ptile_dfr", replace_fn_names(ptile_dfr, list(ptile = "future_ptile")))
assign("future_tile_dfc", replace_fn_names(tile_dfc, list(tile = "future_tile")))
assign("future_tile2_dfc", replace_fn_names(tile2_dfc, list(tile2 = "future_tile2")))
assign("future_ptile_dfc", replace_fn_names(ptile_dfc, list(ptile = "future_ptile")))
for (type in c("lgl", "chr", "int", "dbl")) {
  assign(
    paste0("future_tile_", type),
    replace_fn_names(tile, list(map = paste0("future_map_", type)), ns = "furrr")
  )
  assign(
    paste0("future_tile2_", type),
    replace_fn_names(tile2, list(map2 = paste0("future_map2_", type)), ns = "furrr")
  )
  assign(
    paste0("future_ptile_", type),
    replace_fn_names(ptile, list(pmap = paste0("future_pmap_", type)), ns = "furrr")
  )
}
# nocov end
