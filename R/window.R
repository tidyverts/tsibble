# nocov start
replace_fn_names <- function(fn, replace = list()){
  rec_fn <- function(cl) {
    if (!is_call(cl)) {
      return(cl)
    }
    if (any(repl_fn <- names(replace) %in% call_name(cl))) {
      cl[[1]] <- replace[[repl_fn]]
    }
    as.call(append(cl[[1]], purrr::map(as.list(cl[-1]), rec_fn)))
  }
  body(fn) <- rec_fn(body(fn))
  fn
}
# nocov end

#' Sliding window calculation
#'
#' Rolling window with overlapping observations:
#' * `slide()`, `slide_if()` & `slide_at()` always returns a list.
#' * `slide_lgl()`, `slide_int()`, `slide_dbl()`, `slide_chr()` return vectors
#' of the corresponding type.
#' * `slide_dfr()` `slide_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams purrr::map
#' @param .size An integer for window size.
#' @param .fill A single value or data frame to replace `NA`.
#'
#' @rdname slide
#' @export
#' @seealso
#' * [slide2], [pslide]
#' * [tile] for tiling window without overlapping observations
#' * [stretch] for expanding more observations
#' @details The `slide()` function attempts to tackle more general problems using
#' the purrr-like syntax. For some specialist functions like `mean` and `sum`,
#' you may like to check out for
#' [RcppRoll](https://CRAN.R-project.org/package=RcppRoll) for faster performance.
#'
#' @examples
#' # sliding through a vector ----
#' x <- 1:10
#' slide_dbl(x, mean, .size = 3)
#' slide_dbl(x, mean, .size = 3, .fill = 0)
#' slide_lgl(x, ~ mean(.) > 1, .size = 3)
#'
#' \dontrun{
#' # takes a little longer for cran check
#' # sliding a 2-day window for a data frame ----
#' jan <- pedestrian %>%
#'   filter(Date <= as.Date("2015-01-31")) %>%
#'   split_by(Sensor)
#' # returns a data frame of fitted values and residuals for each sensor,
#' # and then combines
#' diag_jan <- jan %>%
#'   purrr::map_dfr(
#'     ~ slide_dfr(., function(x) {
#'       fit <- lm(Count ~ Time, data = x)
#'       data.frame(fitted = fitted(fit), resid = residuals(fit))
#'     }, .size = 48)
#'   )
#' diag_jan[48:57, ]
#' # save lm models as additional columns
#' lm_jan <- jan %>%
#'   purrr::map(
#'     ~ mutate(., lm = slide(., ~ lm(Count ~ Time, data = .), .size = 48)
#'   ))
#' lm_jan[[1]][48:57, ]
#' }
#' @export
slide <- function(.x, .f, ..., .size = 1, .fill = NA) {
  lst_x <- slider(.x, .size = .size)
  result <- purrr::map(lst_x, .f, ...)
  if (is.na(.fill)) {
    .fill <- rep_len(.fill, length(result[[1]]))
  }
  c(replicate(n = .size - 1, .fill, simplify = FALSE), result)
}

# #' @rdname slide
# #' @export
# slide_if <- function(.x, .p, .f, ..., .size = 1, .fill = NA) {
#   lst_x <- flatten(slider(.x, .size = .size))
#   names(lst_x) <- names(.x)
#   result <- purrr::map_if(lst_x, .p, .f, ...)
#   if (is.na(.fill)) {
#     .fill <- rep_len(.fill, length(result[[1]]))
#   }
#   c(replicate(n = .size - 1, .fill, simplify = FALSE), result)
# }

# #' @rdname slide
# #' @export
# slide_at <- function(.x, .at, .f, ..., .size = 1, .fill = NA) {
#   lst_x <- flatten(slider(.x, .size = .size))
#   names(lst_x) <- names(.x)
#   result <- purrr::map_at(lst_x, .at, .f, ...)
#   if (is.na(.fill)) {
#     .fill <- rep_len(.fill, length(result[[1]]))
#   }
#   c(replicate(n = .size - 1, .fill, simplify = FALSE), result)
# }

#' @rdname slide
#' @export
slide_dbl <- function(.x, .f, ..., .size = 1, .fill = NA) {
  lst_x <- slider(.x, .size = .size)
  c(rep_len(.fill, .size - 1), purrr::map_dbl(lst_x, .f, ...))
}

#' @evalRd paste0('\\alias{slide_', c("lgl", "chr", "int"), '}')
#' @name slide
#' @rdname slide
#' @exportPattern ^slide_
for(type in c("lgl", "chr", "int")){
  assign(
    paste0("slide_", type),
    replace_fn_names(slide_dbl, list(map_dbl = sym(paste0("map_", type))))
  )
}

#' @rdname slide
#' @export
slide_dfr <- function(.x, .f, ..., .size = 1, .fill = NA, .id = NULL) {
  out <- slide(.x, .f = .f, ..., .size = .size, .fill = .fill)
  out_named <- purrr::map(out, `names<-`, names(out[[.size]]))
  dplyr::bind_rows(!!! out_named, .id = .id)
}

#' @rdname slide
#' @export
slide_dfc <- function(.x, .f, ..., .size = 1, .fill = NA) {
  out <- slide(.x, .f = .f, ..., .size = .size, .fill = .fill)
  out_named <- purrr::map(out, `names<-`, names(out[[.size]]))
  dplyr::bind_cols(!!! out_named)
}

#' Sliding window calculation over multiple inputs simultaneously
#'
#' @param .x,.y A vector of numerics, or data frame. If a data frame, row-wise
#' rolling window is performed.
#' @inheritParams slide
#'
#' @rdname slide2
#' @export
#' @seealso
#' * [slide]
#' * [tile2] for tiling window without overlapping observations
#' * [stretch2] for expanding more observations
#'
#' @export
#' @examples
#' x <- 1:10
#' y <- 10:1
#' z <- x * 2
#' slide2_dbl(x, y, cor, .size = 3)
#' slide2(x, y, cor, .size = 3)
#' pslide_dbl(list(x, y, z), sum, .size = 3)
#' @rdname slide2
#' @export
slide2 <- function(.x, .y, .f, ..., .size = 1, .fill = NA) {
  lst <- slider(.x, .y, .size = .size)
  result <- purrr::map2(lst[[1]], lst[[2]], .f, ...)
  if (is.na(.fill)) {
    .fill <- rep_len(.fill, length(result[[1]]))
  }
  c(replicate(n = .size - 1, .fill, simplify = FALSE), result)
}

#' @rdname slide2
#' @export
slide2_dbl <- function(.x, .y, .f, ..., .size = 1, .fill = NA) {
  lst <- slider(.x, .y, .size = .size)
  c(rep_len(.fill, .size - 1), purrr::map2_dbl(lst[[1]], lst[[2]], .f, ...))
}

#' @evalRd paste0('\\alias{slide2_', c("lgl", "chr", "int"), '}', collapse = '\n')
#' @name slide2
#' @rdname slide2
#' @exportPattern ^slide2_
for(type in c("lgl", "chr", "int")){
  assign(
    paste0("slide2_", type),
    replace_fn_names(slide2_dbl, list(map2_dbl = sym(paste0("map2_", type))))
  )
}

#' @rdname slide2
#' @export
slide2_dfr <- function(.x, .y, .f, ..., .size = 1, .fill = NA, .id = NULL) {
  out <- slide(.x, .y, .f = .f, ..., .size = .size, .fill = .fill)
  out_named <- purrr::map(out, `names<-`, names(out[[.size]]))
  dplyr::bind_rows(!!! out_named, .id = .id)
}

#' @rdname slide2
#' @export
slide2_dfc <- function(.x, .f, ..., .size = 1, .fill = NA) {
  out <- slide(.x, .f = .f, ..., .size = .size, .fill = .fill)
  out_named <- purrr::map(out, `names<-`, names(out[[.size]]))
  dplyr::bind_cols(!!! out_named)
}

#' @rdname slide2
#' @inheritParams purrr::pmap
#' @export
pslide <- function(.l, .f, ..., .size = 1, .fill = NA) {
  lst <- slider(.l, .size = .size)
  result <- purrr::pmap(lst, .f, ...)
  if (is.na(.fill)) {
    .fill <- rep_len(.fill, length(result[[1]]))
  }
  c(replicate(n = .size - 1, .fill, simplify = FALSE), result)
}

#' @rdname slide2
#' @export
pslide_dbl <- function(.l, .f, ..., .size = 1, .fill = NA) {
  lst <- slider(.l, .size = .size)
  c(rep_len(.fill, .size - 1), purrr::pmap_dbl(lst, .f, ...))
}

#' @evalRd paste0('\\alias{pslide_', c("lgl", "chr", "int"), '}', collapse = '\n')
#' @name pslide
#' @rdname slide2
#' @exportPattern ^pslide_
for(type in c("lgl", "chr", "int")){
  assign(
    paste0("pslide_", type),
    replace_fn_names(pslide_dbl, list(pmap_dbl = sym(paste0("pmap_", type))))
  )
}

#' @rdname slide2
#' @export
pslide_dfr <- function(.l, .f, ..., .size = 1, .fill = NA, .id = NULL) {
  out <- pslide(.l, .f = .f, ..., .size = .size, .fill = .fill)
  out_named <- purrr::pmap(out, `names<-`, names(out[[.size]]))
  dplyr::bind_rows(!!! out_named, .id = .id)
}

#' @rdname slide2
#' @export
pslide_dfc <- function(.l, .f, ..., .size = 1, .fill = NA) {
  out <- pslide(.l, .f = .f, ..., .size = .size, .fill = .fill)
  out_named <- purrr::pmap(out, `names<-`, names(out[[.size]]))
  dplyr::bind_cols(!!! out_named)
}

#' Splits the input to a list according to the rolling window .size.
#'
#' @param ... Objects to be splitted.
#' @inheritParams slide
#' @rdname slider
#' @export
#' @examples
#' x <- 1:10
#' slider(x, .size = 3)
#' y <- 10:1
#' slider(x, y, .size = 3)
slider <- function(..., .size = 1) {
  x <- flatten(rlang::list2(...))
  if (has_length(x, 1)) {
    return(slider_base(x[[1]], .size = .size))
  } else {
    return(unname(purrr::map(x, ~ slider_base(., .size = .size))))
  }
}

slider_base <- function(x, .size = 1) {
  bad_window_function(x, .size)
  len_x <- NROW(x)
  lst_idx <- seq_len(len_x - .size + 1)
  if (is_atomic(x)) {
    return(purrr::map(lst_idx, ~ x[(.):(. + .size - 1)]))
  }
  purrr::map(lst_idx, ~ x[(.):(. + .size - 1), , drop = FALSE])
}

#' Tiling window calculation
#'
#' Tiling window without overlapping observations:
#' * `tile()`, `tile_if()` & `tile_at()` always returns a list.
#' * `tile_lgl()`, `tile_int()`, `tile_dbl()`, `tile_chr()` return vectors
#' of the corresponding type.
#' * `tile_dfr()` `tile_dfc()` return data frames using row-binding & column-binding.
#'
#' @inheritParams slide
#'
#' @rdname tile
#' @export
#' @seealso
#' * [tile2], [ptile]
#' * [slide] for sliding window with overlapping observations
#' * [stretch] for expanding more observations
#'
#' @examples
#' # tiling over a vector ----
#' x <- 1:10
#' tile_dbl(x, sum, .size = 3)
#' tile_dbl(x, ~ sum(.), .size = 3)
#'
#' # tiling over a 2-day window for hourly data ----
#' \dontrun{
#' pedestrian %>%
#'   split_by(Sensor) %>%
#'   purrr::map_dfr(~ tile_dfr(., ~ quantile(.$Count), .size = 48))
#' }
tile <- function(.x, .f, ..., .size = 1) {
  lst_x <- tiler(.x, .size = .size)
  purrr::map(lst_x, .f, ...)
}

# #' @rdname tile
# #' @export
# tile_if <- function(.x, .p, .f, ..., .size = 1) {
#   lst_x <- flatten(tiler(.x, .size = .size))
#   names(lst_x) <- names(.x)
#   purrr::map_if(lst_x, .p, .f, ...)
# }

# #' @rdname tile
# #' @export
# tile_at <- function(.x, .at, .f, ..., .size = 1) {
#   lst_x <- flatten(tiler(.x, .size = .size))
#   names(lst_x) <- names(.x)
#   purrr::map_at(lst_x, .at, .f, ...)
# }

#' @evalRd paste0('\\alias{tile_', c("lgl", "chr", "dbl", "int"), '}', collapse = '\n')
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
  out <- tile(.x = .x, .f = .f, ..., .size = .size)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname tile
#' @export
tile_dfc <- function(.x, .f, ..., .size = 1) {
  out <- tile(.x = .x, .f = .f, ..., .size = .size)
  dplyr::bind_cols(!!! out)
}

#' Tiling window calculation over multiple inputs simultaneously
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
  out <- tile2(.x, .y, .f = .f, ..., .size = .size)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname tile2
#' @export
tile2_dfc <- function(.x, .y, .f, ..., .size = 1) {
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
  out <- tile(.l, .f = .f, ..., .size = .size)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname tile2
#' @export
ptile_dfc <- function(.l, .f, ..., .size = 1) {
  out <- tile(.l, .f = .f, ..., .size = .size)
  dplyr::bind_cols(!!! out)
}

#' @rdname slider
#' @export
#' @examples
#' x <- 1:10
#' tiler(x, .size = 3)
#' y <- 10:1
#' tiler(x, y, .size = 3)
tiler <- function(..., .size = 1) {
  x <- flatten(rlang::list2(...))
  if (has_length(x, 1)) {
    return(tiler_base(x[[1]], .size = .size))
  } else {
    return(unname(purrr::map(x, ~ tiler_base(., .size = .size))))
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

#' Stretching window calculation
#'
#' Fixing an initial window and expanding more observations:
#' * `stretch()`, `stretch_if()` & `stretch_at()` always returns a list.
#' * `stretch_lgl()`, `stretch_int()`, `stretch_dbl()`, `stretch_chr()` return vectors
#' of the corresponding type.
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
#' x <- 1:10
#' stretch(x, mean, .init = 3)
#' stretch_dbl(x, ~ mean(.), .init = 3)
#'
#' # stretching a 2-day window for a data frame ----
#' sx <- pedestrian %>%
#'   filter(Sensor == "Southern Cross Station", Date <= as.Date("2015-01-10"))
#' sx %>%
#'   stretch_dfr(~ quantile(.$Count), .init = 48)
stretch <- function(.x, .f, ..., .size = 1, .init = 1) {
  lst_x <- stretcher(.x, .size = .size, .init = .init)
  purrr::map(lst_x, .f, ...)
}

# #' @rdname stretch
# #' @export
# stretch_if <- function(.x, .p, .f, ..., .size = 1, .init = 1) {
#   lst_x <- stretcher(.x, .size = .size, .init = .init) %>% 
#     lapply(unlist, recursive = FALSE, use.names = FALSE)
#   names(lst_x) <- names(.x)
#   purrr::map_if(lst_x, .p, .f, ...)
# }
#
# #' @rdname stretch
# #' @export
# stretch_at <- function(.x, .at, .f, ..., .size = 1, .init = 1) {
#   lst_x <- stretcher(.x, .size = .size, .init = .init) %>% 
#     lapply(unlist, recursive = FALSE, use.names = FALSE)
#   names(lst_x) <- names(.x)
#   purrr::map_at(lst_x, .at, .f, ...)
# }

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
  lst <- stretcher(.x, .y, .size = .size, .init = .init)
  purrr::map2(lst[[1]], lst[[2]], .f, ...)
}

#' @evalRd paste0('\\alias{stretch2_', c("lgl", "chr", "dbl", "int"), '}', collapse = '\n')
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
  lst <- stretcher(.l, .size = .size, .init = .init)
  purrr::pmap(lst, .f, ...)
}

#' @evalRd paste0('\\alias{pstretch_', c("lgl", "chr", "dbl", "int"), '}', collapse = '\n')
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
  out <- stretch(.l, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_rows(!!! out, .id = .id)
}

#' @rdname stretch2
#' @export
pstretch_dfc <- function(.l, .f, ..., .size = 1, .init = 1) {
  out <- stretch(.l, .f = .f, ..., .size = .size, .init = .init)
  dplyr::bind_cols(!!! out)
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
  x <- flatten(rlang::list2(...))
  if (has_length(x, 1)) {
    return(stretcher_base(x[[1]], .size = .size, .init = .init))
  } else {
    return(unname(purrr::map(x, ~ stretcher_base(., .size = .size, .init = .init))))
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
  purrr::map(incr_lst, ~ x[., , drop = FALSE])
}

bad_window_function <- function(.x, .size) {
  if (is_bare_list(.x)) {
    abort("`.x` must not be a list of lists.")
  }
  if (!is_bare_numeric(.size, n = 1) || .size < 1) {
    abort("`.size` must be a positive integer.")
  }
}

incr <- function(init, size) {
  init
  function() {
    init <<- init + size
    init
  }
}
