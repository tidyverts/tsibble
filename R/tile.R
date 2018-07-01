#' Tiling window calculation
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
#' @seealso
#' * [tile2], [ptile], [ltile]
#' * [slide] for sliding window with overlapping observations
#' * [stretch] for expanding more observations
#'
#' @examples
#' # tiling over a vector ----
#' x <- 1:10
#' tile_dbl(x, sum, .size = 3)
#' tile_dbl(x, ~ sum(.), .size = 3)
tile <- function(.x, .f, ..., .size = 1) {
  only_atomic(.x)
  lst_x <- tiler(.x, .size = .size)
  purrr::map(lst_x, .f, ...)
}

#' @evalRd paste0('\\alias{tile_', c("lgl", "chr", "dbl", "int"), '}')
#' @name tile
#' @rdname tile
#' @exportPattern ^tile_
for(type in c("lgl", "chr", "dbl", "int")){
  assign(
    paste0("tile_", type),
    replace_fn_names(tile, list(map = sym(paste0("map_", type))))
  )
}

#' @rdname tile
#' @export
tile_dfr <- function(.x, .f, ..., .size = 1, .id = NULL) {
  only_atomic(.x)
  out <- tile(.x = .x, .f = .f, ..., .size = .size)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname tile
#' @export
tile_dfc <- function(.x, .f, ..., .size = 1) {
  only_atomic(.x)
  out <- tile(.x = .x, .f = .f, ..., .size = .size)
  dplyr::bind_cols(!!! out)
}

#' Tiling window calculation over multiple inputs simultaneously
#'
#' Tiling window without overlapping observations:
#' * `tile2()` and `ptile()` always returns a list.
#' * `tile2_lgl()`, `tile2_int()`, `tile2_dbl()`, `tile2_chr()` use the same
#' arguments as `tile2()`, but return vectors of the corresponding type.
#' * `tile2_dfr()` `tile2_dfc()` return data frames using row-binding & column-binding.
#'
#' @param .x,.y A vector of numerics, or data frame. If a data frame, row-wise rolling
#' window is performed.
#'
#' @inheritParams slide2
#'
#' @rdname tile2
#' @export
#' @seealso
#' * [tile]
#' * [slide2] for sliding window with overlapping observations
#' * [stretch2] for expanding more observations
#'
#' @examples
#' x <- 1:10
#' y <- 10:1
#' z <- x * 2
#' tile2_dbl(x, y, cor, .size = 5)
#' tile2(x, y, ~ cor(.x, .y), .size = 5)
#' ptile(list(x, y, z), sum, .size = 5)
tile2 <- function(.x, .y, .f, ..., .size = 1) {
  only_atomic(.x)
  only_atomic(.y)
  lst <- tiler(.x, .y, .size = .size)
  purrr::map2(lst[[1]], lst[[2]], .f, ...)
}

#' @evalRd paste0('\\alias{tile2_', c("lgl", "chr", "dbl", "int"), '}', collapse = '\n')
#' @name tile2
#' @rdname tile2
#' @exportPattern ^tile2_
for(type in c("lgl", "chr", "dbl", "int")){
  assign(
    paste0("tile2_", type),
    replace_fn_names(tile2, list(map2 = sym(paste0("map2_", type))))
  )
}

#' @rdname tile2
#' @export
tile2_dfr <- function(.x, .y, .f, ..., .size = 1, .id = NULL) {
  only_atomic(.x)
  only_atomic(.y)
  out <- tile2(.x, .y, .f = .f, ..., .size = .size)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname tile2
#' @export
tile2_dfc <- function(.x, .y, .f, ..., .size = 1) {
  only_atomic(.x)
  only_atomic(.y)
  out <- tile2(.x, .y, .f = .f, ..., .size = .size)
  dplyr::bind_cols(!!! out)
}

#' @rdname tile2
#' @export
ptile <- function(.l, .f, ..., .size = 1) {
  lst <- tiler(.l, .size = .size)
  purrr::pmap(lst, .f, ...)
}

#' @evalRd paste0('\\alias{ptile_', c("lgl", "chr", "dbl", "int"), '}')
#' @name ptile
#' @rdname tile2
#' @exportPattern ^ptile_
for(type in c("lgl", "chr", "dbl", "int")){
  assign(
    paste0("ptile_", type),
    replace_fn_names(ptile, list(pmap = sym(paste0("pmap_", type))))
  )
}

#' @rdname tile2
#' @export
ptile_dfr <- function(.l, .f, ..., .size = 1, .id = NULL) {
  lst <- tiler(.l, .size = .size)
  out <- purrr::pmap(lst, .f, ...)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname tile2
#' @export
ptile_dfc <- function(.l, .f, ..., .size = 1) {
  lst <- tiler(.l, .size = .size)
  out <- purrr::pmap(lst, .f, ...)
  dplyr::bind_cols(!!! out)
}

#' Tiling window on a list
#'
#' @inheritParams purrr::lmap
#' @inheritParams tile
#' @rdname ltile
#' @export
ltile <- function(.x, .f, ..., .size = 1) {
  only_list(.x)
  lst <- tiler(.x, .size = .size)
  ltile_constructor(lst, .f, ...)
}

#' @rdname ltile
#' @export
ltile_if <- function(.x, .p, .f, ..., .size = 1) {
  only_list(.x)
  sel <- probe(.x, .p)
  out <- list_along(.x)
  lst <- tiler(.x[sel], .size = .size)
  out[sel] <- ltile_constructor(lst, .f, ...)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

#' @rdname ltile
#' @export
ltile_at <- function(.x, .at, .f, ..., .size = 1) {
  only_list(.x)
  sel <- inv_which(.x, .at)
  out <- list_along(.x)
  lst <- tiler(.x[sel], .size = .size)
  out[sel] <- ltile_constructor(lst, .f, ...)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

ltile_constructor <- function(x, .f, ...) {
  purrr::modify_depth(x, 2, .f, ...) %>% 
    purrr::map(unlist, recursive = FALSE, use.names = FALSE)
}

#' @rdname slider
#' @export
#' @examples
#' x <- 1:10
#' tiler(x, .size = 3)
#' y <- 10:1
#' tiler(x, y, .size = 3)
tiler <- function(..., .size = 1) {
  lst <- list2(...)
  x <- flatten(lst)
  if (purrr::vec_depth(lst) == 2 && has_length(x, 1)) {
    return(tiler_base(x[[1]], .size = .size))
  } else {
    return(purrr::map(x, ~ tiler_base(., .size = .size)))
  }
}

tiler_base <- function(x, .size = 1) {
  bad_window_function(x, .size)
  len_x <- NROW(x)
  seq_x <- seq_len(len_x)
  denom <- len_x + 1
  frac <- ceiling((seq_x %% denom) / .size)
  unname(split(x, frac, drop = TRUE))
}

