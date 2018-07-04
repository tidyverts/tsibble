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
#' * [stretch2], [pstretch]
#' * [slide] for sliding window with overlapping observations
#' * [tile] for tiling window without overlapping observations
#'
#' @examples
#' .x <- 1:5
#' .lst <- list(x = .x, y = 6:10, z = 11:15)
#' stretch_dbl(.x, mean, .size = 2)
#' stretch_lgl(.x, ~ mean(.) > 2, .size = 2)
#' stretch(.lst, ~ ., .size = 2)
stretch <- function(.x, .f, ..., .size = 1, .init = 1) {
  if (is_list(.x)) {
    lst_x <- stretcher(.x, .size = .size, .init = .init) %>% 
      purrr::modify_depth(1, unlist2)
  } else {
    lst_x <- stretcher(.x, .size = .size, .init = .init)
  }
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
  out <- stretch(.x, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch
#' @export
stretch_dfc <- function(.x, .f, ..., .size = 1, .init = 1) {
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
#' .x <- 1:5
#' .y <- 6:10
#' .z <- 11:15
#' .lst <- list(x = .x, y = .y, z = .z)
#' .df <- as.data.frame(.lst)
#' stretch2(.x, .y, sum, .size = 2)
#' stretch2(.lst, .lst, sum, .size = 2)
#' stretch2(.df, .df, sum, .size = 2)
#' pstretch(.lst, sum, size = 1)
#' pstretch(list(.lst, .lst), ~ ., .size = 2)
stretch2 <- function(.x, .y, .f, ..., .size = 1, .init = 1) {
  if (is_list(.x)) {
    lst <- pstretcher(.x, .y, .size = .size, .init = .init) %>% 
      purrr::modify_depth(2, unlist2)
  } else {
    lst <- pstretcher(.x, .y, .size = .size, .init = .init)
  }
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
  out <- stretch(.x, .y, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch2
#' @export
stretch2_dfc <- function(.x, .y, .f, ..., .size = 1, .init = 1) {
  out <- stretch(.x, .y, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_cols(!!! out)
}

#' @rdname stretch2
#' @export
pstretch <- function(.l, .f, ..., .size = 1, .init = 1) {
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  depth <- purrr::vec_depth(.l)
  if (depth == 2) { # a list of multiple elements
    lst <- pstretcher(!!! .l, .size = .size, .init = .init) %>%  # slide simultaneously
      purrr::modify_depth(2, unlist2)
  } else if (depth == 3) { # a list of lists
    lst <- pstretcher(!!! .l, .size = .size, .init = .init) %>% 
      purrr::modify_depth(3, unlist2)
  }
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

#' Splits the input to a list according to the stretching window size.
#'
#' @param x An objects to be splitted.
#' @param ... Multiple objects to be splitted in parallel.
#' @inheritParams stretch
#' @rdname stretcher
#' @export
#' @examples
#' .x <- 1:5
#' .y <- 6:10
#' .z <- 11:15
#' .lst <- list(x = .x, y = .y, z = .z)
#' .df <- as.data.frame(.lst)
#'
#' stretcher(.x, .size = 2)
#' stretcher(.lst, .size = 2)
#' pstretcher(.lst, .size = 2)
#' stretcher(.df, .size = 2)
#' pstretcher(.df, .df, .size = 2)
stretcher <- function(.x, .size = 1, .init = 1) {
  if (is_atomic(.x)) {
    return(stretcher_base(.x, .size = .size, .init = .init))
  } else {
    if (is.data.frame(.x)) .x <- as.list(.x)
    return(stretcher_base(.x, .size = .size, .init = .init))
  }
}

#' @rdname stretcher
#' @export
pstretcher <- function(..., .size = 1, .init = 1) { # parallel sliding
  .x <- list2(...)
  depth <- purrr::vec_depth(.x)
  if (depth == 2) {
    return(purrr::map(.x, 
      function(x) stretcher_base(x, .size = .size, .init = .init)
    ))
  } else if (depth == 3) { # a list of lists
    df_lgl <- purrr::map_lgl(.x, is.data.frame)
    if (any(df_lgl)) {
      .x[df_lgl] <- purrr::map(.x[df_lgl], as.list)
    }
    return(purrr::map(.x, 
      function(x) stretcher_base(x, .size = .size, .init = .init)
    ))
  } else {
    abort("Must not be deeper than 3.")
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
  purrr::map(incr_lst, ~ x[.])
}
