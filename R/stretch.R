#' Stretching window calculation
#'
#' Fixing an initial window and expanding more observations:
#' * `stretch()` always returns a list.
#' * `stretch_lgl()`, `stretch_int()`, `stretch_dbl()`, `stretch_chr()` use the same
#' arguments as `stretch()`, but return vectors of the corresponding type.
#' * `stretch_dfr()` `stretch_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide
#' @param .size,.init An integer for moving and initial window size.
#'
#' @rdname stretch
#' @export
#' @seealso
#' * [stretch2], [pstretch], [lstretch]
#' * [slide] for sliding window with overlapping observations
#' * [tile] for tiling window without overlapping observations
#'
#' @examples
#' x <- 1:10
#' stretch(x, mean, .init = 3)
#' stretch_dbl(x, ~ mean(.), .init = 3)
stretch <- function(.x, .f, ..., .size = 1, .init = 1) {
  only_atomic(.x)
  lst_x <- stretcher(.x, .size = .size, .init = .init)
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
stretch_dfr <- function(.x, .f, ..., .size = 1, .init = 1, .id = NULL) {
  only_atomic(.x)
  out <- stretch(.x, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch
#' @export
stretch_dfc <- function(.x, .f, ..., .size = 1, .init = 1) {
  only_atomic(.x)
  out <- stretch(.x, .f = .f, ..., .size = .size, .init = .init)
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
#' x <- 1:10
#' y <- 10:1
#' z <- x * 2
#' stretch2(x, y, cor, .init = 3)
#' stretch2_dbl(x, y, ~ cor(.x, .y), .init = 3)
#' pstretch(list(x, y, z), sum, .init = 3)
stretch2 <- function(.x, .y, .f, ..., .size = 1, .init = 1) {
  only_atomic(.x)
  only_atomic(.y)
  lst <- stretcher(.x, .y, .size = .size, .init = .init)
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
stretch2_dfr <- function(.x, .y, .f, ..., .size = 1, .init = 1, .id = NULL) {
  only_atomic(.x)
  only_atomic(.y)
  out <- stretch(.x, .y, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch2
#' @export
stretch2_dfc <- function(.x, .y, .f, ..., .size = 1, .init = 1) {
  only_atomic(.x)
  only_atomic(.y)
  out <- stretch(.x, .y, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_cols(!!! out)
}

#' @rdname stretch2
#' @export
pstretch <- function(.l, .f, ..., .size = 1, .init = 1) {
  lst <- stretcher(.l, .size = .size, .init = .init)
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
pstretch_dfr <- function(.l, .f, ..., .size = 1, .init = 1, .id = NULL) {
  lst <- stretcher(.l, .size = .size, .init = .init)
  out <- purrr::pmap(lst, .f, ...)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch2
#' @export
pstretch_dfc <- function(.l, .f, ..., .size = 1, .init = 1) {
  lst <- stretcher(.l, .size = .size, .init = .init)
  out <- purrr::pmap(lst, .f, ...)
  dplyr::bind_cols(!!! out)
}

#' Stretching window on a list
#'
#' @inheritParams purrr::lmap
#' @inheritParams stretch
#' @rdname lstretch
#' @export
lstretch <- function(.x, .f, ..., .size = 1, .init = 1) {
  only_list(.x)
  lst <- stretcher(.x, .size = .size, .init = .init)
  lstretch_constructor(lst, .f, ...)
}

#' @rdname lstretch
#' @export
lstretch_if <- function(.x, .p, .f, ..., .size = 1, .init = 1) {
  only_list(.x)
  sel <- probe(.x, .p)
  out <- list_along(.x)
  lst <- stretcher(.x[sel], .size = .size, .init = .init)
  out[sel] <- lstretch_constructor(lst, .f, ...)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

#' @rdname lstretch
#' @export
lstretch_at <- function(.x, .at, .f, ..., .size = 1, .init = 1) {
  only_list(.x)
  sel <- inv_which(.x, .at)
  out <- list_along(.x)
  lst <- stretcher(.x[sel], .size = .size, .init = .init)
  out[sel] <- lstretch_constructor(lst, .f, ...)
  out[!sel] <- .x[!sel]
  set_names(out, names(.x))
}

lstretch_constructor <- function(x, .f, ...) {
  purrr::modify_depth(x, 2, .f, ...) %>% 
    purrr::map(unlist, recursive = FALSE, use.names = FALSE)
}

#' @rdname slider
#' @param .size,.init An integer for moving and initial window size.
#' @export
#' @examples
#' x <- 1:10
#' stretcher(x, .init = 3)
#' y <- 10:1
#' stretcher(x, y, .init = 3)
stretcher <- function(..., .size = 1, .init = 1) {
  lst <- list2(...)
  x <- flatten(lst)
  if (purrr::vec_depth(lst) == 2 && has_length(x, 1)) {
    return(stretcher_base(x[[1]], .size = .size, .init = .init))
  } else {
    return(purrr::map(x, ~ stretcher_base(., .size = .size, .init = .init)))
  }
}

stretcher_base <- function(x, .size = 1, .init = 1) {
  bad_window_function(x, .size)
  if (!is_bare_numeric(.init, n = 1) || .init < 1) {
    abort("`.init` must be a positive integer.")
  }
  counter <- incr(init = .init, size = .size)

  ncall <- seq_len(ceiling((NROW(x) - .init) / .size) - 1)
  incr_lst <- c(
    list(seq_len(.init)),
    purrr::map(ncall, ~ seq_len(counter())),
    list(seq_along(x))
  )
  if (is_atomic(x)) {
    return(purrr::map(incr_lst, function(i) x[i]))
  }
  # purrr::map(incr_lst, ~ x[., , drop = FALSE])
}

