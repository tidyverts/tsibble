#' Sliding window calculation
#'
#' Rolling window with overlapping observations:
#' * `slide()` always returns a vector of numerics
#' * `slide_lst()` returns a list
#' * `slide_dfr()` and `slide_dfc()` return data frame using row-binding and
#' column-binding
#' * `slider()` splits the input `x` to a list according to the window size.
#'
#' @param x A vector of numerics, or data frame. If a data frame, row-wise rolling
#' window is performed.
#' @param .f A function or one-sided formula using purrr-like syntax. If a 
#' formula, it is converted to a function.
#' @param ... Additional arguments passed on to `.f`.
#' @param size An integer for window size.
#' @param fill A single value or data frame to replace `NA`.
#' @param .id If not `NULL` a variable with this name will be created giving 
#' either the name or the index of the data frame, which is passed to 
#' [dplyr::bind_rows].
#'
#' @rdname slide
#' @export
#' @seealso [tile] for tiling window without overlapping observations;
#' [stretch] for expanding more observations
#' @details The `slide()` function attempts to tackle more general problems using
#' the purrr-like syntax. For some specialist functions like `mean` and `sum`, 
#' you may like to check out for 
#' [RcppRoll](https://CRAN.R-project.org/package=RcppRoll) for faster performance.
#'
#' @examples
#' # sliding through a vector ----
#' x <- 1:10
#' slide(x, mean, size = 3)
#' slide(x, ~ mean(.), size = 3)
#' slide(x, mean, size = 3, fill = 0)
#'
#' # slider ----
#' slider(x, size = 3)
#'
#' \dontrun{
#' # takes a little longer for cran check
#' # sliding a 2-day window for a data frame ----
#' jan <- pedestrian %>% 
#'   filter(Date <= as.Date("2015-01-31")) %>% 
#'   split_by(Sensor)
#' # directly return a data frame of fitted values and residuals
#' diag_jan <- jan %>%
#'   purrr::map_dfr(~ slide_dfr(., function(x) {
#'     fit <- lm(Count ~ Time, data = x)
#'     data.frame(fitted = fitted(fit), resid = residuals(fit))
#'   }, size = 48))
#' diag_jan[48:57, ]
#' # save lm models as additional columns
#' lm_jan <- jan %>% 
#'   purrr::map(~ mutate(., 
#'     lm = slide_lst(., ~ lm(Count ~ Time, data = .), size = 48)
#' ))
#' lm_jan[[1]][48:57, ]
#' }
#' @export
slide <- function(x, .f, ..., size = 1, fill = NA_real_) {
  lst_x <- slider(x, size = size)
  c(rep_len(fill, size - 1), purrr::map_dbl(lst_x, .f, ...))
}

#' @rdname slide
#' @export
slide_lst <- function(x, .f, ..., size = 1, fill = NA) {
  lst_x <- slider(x, size = size)
  result <- purrr::map(lst_x, .f, ...)
  if (is.na(fill)) {
    fill <- rep_len(fill, length(result[[1]]))
  }
  c(replicate(n = size - 1, fill, simplify = FALSE), result)
}

#' @rdname slide
#' @export
slide_dfr <- function(x, .f, ..., size = 1, fill = NA, .id = NULL) {
  out <- slide_lst(x, .f = .f, ..., size = size, fill = fill)
  if (is.null(names(out))) {
    return(as_tibble(do.call(rbind, out)))
  }
  dplyr::bind_rows(out, .id = .id)
}

#' @rdname slide
#' @export
slide_dfc <- function(x, .f, ..., size = 1, fill = NA) {
  out <- slide_lst(x, .f = .f, ..., size = size, fill = fill)
  if (is.null(names(out))) {
    return(as_tibble(do.call(cbind, out)))
  }
  dplyr::bind_cols(out)
}

#' @rdname slide
#' @export
slider <- function(x, size = 1) {
  bad_window_function(x, size)
  len_x <- NROW(x)
  lst_idx <- seq_len(len_x - size + 1)
  if (is_atomic(x)) {
    return(purrr::map(lst_idx, ~ x[(.):(. + size - 1)]))
  }
  purrr::map(lst_idx, ~ x[(.):(. + size - 1), , drop = FALSE])
}

#' Tiling window calculation
#'
#' Tiling window without overlapping observations:
#' * `tile()` always returns a vector of numerics
#' * `tile_lst()` returns a list
#' * `tile_dfr()` and `tile_dfc()` return data frame using row-binding and
#' column-binding
#' * `tiler()` splits the input `x` to a list according to the window size.
#'
#' @inheritParams slide
#'
#' @rdname tile
#' @export
#' @seealso [slide] for sliding window with overlapping observations;
#' [stretch] for expanding more observations
#'
#' @examples
#' # tiling over a vector ----
#' x <- 1:10
#' tile(x, sum, size = 3)
#' tile(x, ~ sum(.), size = 3)
#' tiler(x, size = 3)
#'
#' # tiling over a 2-day window for hourly data ----
#' \dontrun{
#' pedestrian %>% 
#'   split_by(Sensor) %>% 
#'   purrr::map_dfr(~ tile_dfr(., ~ quantile(.$Count), size = 48))
#' }
tile <- function(x, .f, ..., size = 1) {
  lst_x <- tiler(x, size = size)
  purrr::map_dbl(lst_x, .f, ...)
}

#' @rdname tile
#' @export
tile_lst <- function(x, .f, ..., size = 1) {
  lst_x <- tiler(x, size = size)
  purrr::map(lst_x, .f, ...)
}

#' @rdname tile
#' @export
tile_dfr <- function(x, .f, ..., size = 1, .id = NULL) {
  out <- tile_lst(x = x, .f = .f, ..., size = size)
  if (is.null(names(out))) {
    return(as_tibble(do.call(rbind, out)))
  }
  dplyr::bind_rows(out, .id = .id)
}

#' @rdname tile
#' @export
tile_dfc <- function(x, .f, ..., size = 1) {
  out <- tile_lst(x = x, .f = .f, ..., size = size)
  if (is.null(names(out))) {
    return(as_tibble(do.call(cbind, out)))
  }
  dplyr::bind_cols(out)
}

#' @rdname tile
#' @export
tiler <- function(x, size = 1) {
  bad_window_function(x, size)
  len_x <- NROW(x)
  seq_x <- seq_len(len_x)
  denom <- len_x + 1
  frac <- ceiling((seq_x %% denom) / size)
  unname(split(x, frac, drop = TRUE))
}

#' Stretching window calculation
#'
#' Fixing an initial window and expanding more observations:
#' * `stretch()` always returns a vector of numerics
#' * `stretch_lst()` returns a list
#' * `stretch_dfr()` and `stretch_dfc()` return data frame using row-binding and
#' column-binding
#' * `stretcher()` splits the input `x` to a list according to the window size.
#'
#' @inheritParams slide
#' @param size,init An integer for moving and initial window size.
#'
#' @rdname stretch
#' @export
#' @seealso [slide] for sliding window with overlapping observations;
#' [tile] for tiling window without overlapping observations.
#'
#' @examples
#' x <- 1:10
#' stretch(x, mean, init = 3)
#' stretch(x, ~ mean(.), init = 3)
#' stretcher(x, init = 3)
#'
#' # stretching a 2-day window for a data frame ----
#' sx <- pedestrian %>% 
#'   filter(Sensor == "Southern Cross Station", Date <= as.Date("2015-01-10"))
#' sx %>%
#'   stretch_dfr(~ quantile(.$Count), init = 48)
stretch <- function(x, .f, ..., size = 1, init = 1) {
  lst_x <- stretcher(x, size = size, init = init)
  purrr::map_dbl(lst_x, .f, ...)
}

#' @rdname stretch
#' @export
stretch_lst <- function(x, .f, ..., size = 1, init = 1) {
  lst_x <- stretcher(x, size = size, init = init)
  purrr::map(lst_x, .f, ...)
}

#' @rdname stretch
#' @export
stretch_dfr <- function(x, .f, ..., size = 1, init = 1, .id = NULL) {
  out <- stretch_lst(x, .f = .f, ..., size = size, init = init)
  if (is.null(names(out))) {
    return(as_tibble(do.call(rbind, out)))
  }
  dplyr::bind_rows(out, .id = .id)
}

#' @rdname stretch
#' @export
stretch_dfc <- function(x, .f, ..., size = 1, init = 1) {
  out <- stretch_lst(x, .f = .f, ..., size = size, init = init)
  if (is.null(names(out))) {
    return(as_tibble(do.call(cbind, out)))
  }
  dplyr::bind_cols(out)
}

#' @rdname stretch
#' @export
stretcher <- function(x, size = 1, init = 1) {
  bad_window_function(x, size)
  if (!is_bare_numeric(init, n = 1) || init < 1) {
    abort("`init` must be a positive integer.")
  }
  incr <- function(init, size) {
    init
    function() {
      init <<- init + size
      init
    }
  }
  counter <- incr(init = init, size = size) 

  ncall <- seq_len(ceiling((NROW(x) - init) / size) - 1)
  incr_lst <- c(
    list(seq_len(init)), 
    purrr::map(ncall, ~ seq_len(counter())),
    list(seq_along(x))
  )
  purrr::map(incr_lst, function(i) x[i])
}

bad_window_function <- function(x, size) {
  if (is_bare_list(x)) {
    abort("`x` must not be a list.")
  }
  if (!is_bare_numeric(size, n = 1) || size < 1) {
    abort("`size` must be a positive integer.")
  }
}
