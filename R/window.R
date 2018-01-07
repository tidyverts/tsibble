#' Sliding window calculation
#'
#' `slide()` is an S3 method to carry out rolling window calculation; `slider()`
#' splits the input `x` to a list according to the window size.
#'
#' @param x A vector of numerics, or data frame.
#' @param .f A function or one-sided formula using purrr-like syntax. If a 
#' formula, it is converted to a function.
#' @param ... Additional arguments passed on to `.f`.
#' @param size An integer for window size.
#' @param fill A single value or data frame to replace `NA`.
#'
#' @rdname slide
#' @export
#' @seealso [tile] for tiling window without overlapping observations;
#' [stretch] for expanding more observations
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
#' sx <- pedestrian %>% 
#'   filter(Sensor == "Southern Cross Station", Date <= as.Date("2015-01-31"))
#' # directly return a data frame of fitted values and residuals
#' diag_sx <- sx %>%
#'   slide(function(x) {
#'     fit <- lm(Count ~ Time, data = x)
#'     data.frame(fitted = fitted(fit), resid = residuals(fit))
#'   }, size = 48, deframe = FALSE)
#' head(diag_sx)
#' # save lm models as additional columns
#' lm_sx <- sx %>% 
#'   mutate(lm = slide(., ~ lm(Count ~ Time, data = .), size = 48))
#' head(lm_sx)
#' }
slide <- function(x, .f, ..., size = 1, fill) {
  UseMethod("slide")
}

#' @rdname slide
#' @details The `slide()` function attempts to tackle more general problems using
#' the purrr-like syntax. For some specialist functions like `mean` and `sum`, 
#' you may like to check out for 
#' [RcppRoll](https://CRAN.R-project.org/package=RcppRoll) for faster performance.
#' @export
slide.default <- function(x, .f, ..., size = 1, fill = NA_real_) {
  lst_x <- slider(x, size = size)
  c(rep_len(fill, size - 1), purrr::map_dbl(lst_x, .f, ...))
}

#' @rdname slide
#' @param deframe TRUE a list is returned. FALSE returns a data frame.
#' @export
slide.data.frame <- function(
  x, .f, ..., size = 1, fill = NA, deframe = TRUE
) {
  # currently not support grouped_df/ts
  lst_x <- slider(x, size = size)
  result <- purrr::map(lst_x, .f, ...)
  if (is.na(fill)) {
    fill <- rep_len(fill, length(result[[1]]))
  }
  out <- c(replicate(n = size - 1, fill, simplify = FALSE), result)
  if (deframe) {
    return(out)
  } else if (is.null(names(out))) {
    return(as_tibble(do.call(rbind, out)))
  } else {
    dplyr::bind_rows(out)
  }
}

#' @rdname slide
#' @export
slider <- function(x, size = 1) {
  bad_window_function(x, size)
  if (is.data.frame(x)) {
    len_x <- nrow(x)
  } else {
    len_x <- length(x)
  }
  lst_idx <- seq_len(len_x - size + 1)
  if (is_atomic(x)) {
    return(purrr::map(lst_idx, ~ x[(.):(. + size - 1)]))
  }
  purrr::map(lst_idx, ~ x[(.):(. + size - 1), , drop = FALSE])
}

#' Tiling window calculation
#'
#' `tile()` is an S3 method to carry out tiling window calculation; `tiler()`
#' splits the input `x` to a list according to the window size.
#'
#' @param x A vector of numerics, or data frame.
#' @inheritParams slide
#' @param ... Additional arguments passed on to `.f`.
#' @param size An integer for window size.
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
#' sx <- pedestrian %>%
#'   filter(Sensor == "Southern Cross Station", Date <= as.Date("2015-01-10"))
#' sx %>% 
#'   tile(~ quantile(.$Count), size = 48, deframe = FALSE)
tile <- function(x, .f, ..., size = 1) {
  UseMethod("tile")
}

#' @rdname tile
#' @export
tile.default <- function(x, .f, ..., size = 1) {
  lst_x <- tiler(x, size = size)
  purrr::map_dbl(lst_x, .f, ...)
}

#' @rdname tile
#' @param deframe TRUE a list is returned. FALSE returns a data frame.
#' @export
tile.data.frame <- function(x, .f, ..., size = 1, deframe = TRUE) {
  lst_x <- tiler(x, size = size)
  out <- purrr::map(lst_x, .f, ...)
  if (deframe) {
    return(out)
  } else if (is.null(names(out))) {
    return(as_tibble(do.call(rbind, out)))
  } else {
    dplyr::bind_rows(out)
  }
}

#' @rdname tile
#' @export
tiler <- function(x, size = 1) {
  bad_window_function(x, size)
  len_x <- NROW(x)
  seq_x <- seq_len(len_x)
  denom <- len_x + 1
  frac <- ceiling((seq_x %% denom) / size)
  unname(split(x, frac))
}

#' Stretching window calculation
#'
#' `stretch()` is an S3 method to carry out expanding window calculation; 
#' `stretcher()` splits the input `x` to a list according to the window size.
#'
#' @param x A vector of numerics, or data frame.
#' @inheritParams slide
#' @param ... Additional arguments passed on to `.f`.
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
#'   stretch(~ quantile(.$Count), init = 48, deframe = FALSE)
stretch <- function(x, .f, ..., size = 1, init = 1) {
  UseMethod("stretch")
}

#' @rdname stretch
#' @export
stretch.default <- function(x, .f, ..., size = 1, init = 1) {
  lst_x <- stretcher(x, size = size, init = init)
  purrr::map_dbl(lst_x, .f, ...)
}

#' @rdname stretch
#' @param deframe TRUE a list is returned. FALSE returns a data frame.
#' @export
stretch.data.frame <- function(x, .f, ..., size = 1, init = 1, deframe = TRUE) {
  lst_x <- stretcher(x, size = size, init = init)
  out <- purrr::map(lst_x, .f, ...)
  if (deframe) {
    return(out)
  } else if (is.null(names(out))) {
    return(as_tibble(do.call(rbind, out)))
  } else {
    dplyr::bind_rows(out)
  }
}

#' @rdname stretch
#' @export
stretcher <- function(x, size = 1, init = 1) {
  bad_window_function(x, size)
  if (!is_bare_numeric(init, n = 1) || init < 1) {
    abort("The initial window must be a positive integer.")
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
    abort("Unsupported input class: list")
  }
  if (!is_bare_numeric(size, n = 1) || size < 1) {
    abort("The window size must be a positive integer.")
  }
}
