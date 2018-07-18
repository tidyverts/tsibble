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
#' * `slide()` always returns a list.
#' * `slide_lgl()`, `slide_int()`, `slide_dbl()`, `slide_chr()` use the same
#' arguments as `slide()`, but return vectors of the corresponding type.
#' * `slide_dfr()` `slide_dfc()` return data frames using row-binding & column-binding.
#'
#' @param .x An object to slide over.
#' @inheritParams purrr::map
#' @param .size An integer for window size. If positive, moving forward from left
#' to right; if negative, moving backward (from right to left).
#' @param .fill A value to fill at the left of the data range (`NA` by default).
#' `NULL` means no filling.
#' @param .partial if `TRUE`, partial sliding.
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
#' x <- 1:5
#' lst <- list(x = x, y = 6:10, z = 11:15)
#' slide_dbl(x, mean, .size = 2)
#' slide_lgl(x, ~ mean(.) > 2, .size = 2)
#' slide(lst, ~ ., .size = 2)
slide <- function(.x, .f, ..., .size = 1, .fill = NA, .partial = FALSE) {
  lst_x <- slider(.x, .size = .size, .fill = .fill, .partial = .partial)
  out <- purrr::map(lst_x, .f, ...)
  if (.partial) return(out)
  pad_slide(out, .size, .fill)
}

#' @evalRd paste0('\\alias{slide_', c("lgl", "chr", "int", "dbl"), '}')
#' @exportPattern ^slide_
for(type in c("lgl", "chr", "int", "dbl")){
  assign(
    paste0("slide_", type),
    replace_fn_names(slide, list(map = sym(paste0("map_", type))))
  )
}

#' @rdname slide
#' @export
slide_dfr <- function(
  .x, .f, ..., .size = 1, .fill = NA, .partial = FALSE, .id = NULL
) {
  out <- slide(
    .x, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial
  )
  bind_df(out, .size, .fill, .id = .id)
}

#' @rdname slide
#' @export
slide_dfc <- function(.x, .f, ..., .size = 1, .fill = NA, .partial = FALSE) {
  out <- slide(
    .x, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' Sliding window calculation over multiple inputs simultaneously
#'
#' Rolling window with overlapping observations:
#' * `slide2()` and `pslide()` always returns a list.
#' * `slide2_lgl()`, `slide2_int()`, `slide2_dbl()`, `slide2_chr()` use the same
#' arguments as `slide2()`, but return vectors of the corresponding type.
#' * `slide2_dfr()` `slide2_dfc()` return data frames using row-binding & column-binding.
#'
#' @param .x,.y Objects to slide over simultaneously.
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
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' lst <- list(x = x, y = y, z = z)
#' df <- as.data.frame(lst)
#' slide2(x, y, sum, .size = 2)
#' slide2(lst, lst, ~ ., .size = 2)
#' slide2(df, df, ~ ., .size = 2)
#' pslide(lst, ~ ., .size = 1)
#' pslide(list(lst, lst), ~ ., .size = 2)
slide2 <- function(.x, .y, .f, ..., .size = 1, .fill = NA, .partial = FALSE) {
  lst <- pslider(.x, .y, .size = .size, .fill = .fill, .partial = .partial)
  out <- purrr::map2(lst[[1]], lst[[2]], .f = .f, ...)
  if (.partial) return(out)
  pad_slide(out, .size, .fill)
}

#' @evalRd paste0('\\alias{slide2_', c("lgl", "chr", "int", "dbl"), '}')
#' @exportPattern ^slide2_
for(type in c("lgl", "chr", "int", "dbl")){
  assign(
    paste0("slide2_", type),
    replace_fn_names(slide2, list(map2 = sym(paste0("map2_", type))))
  )
}

#' @rdname slide2
#' @export
slide2_dfr <- function(
  .x, .y, .f, ..., .size = 1, .fill = NA, .partial = FALSE, .id = NULL
) {
  out <- slide2(
    .x, .y, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial
  )
  bind_df(out, .size, .fill, .id = .id)
}

#' @rdname slide2
#' @export
slide2_dfc <- function(
  .x, .y, .f, ..., .size = 1, .fill = NA, .partial = FALSE
) {
  out <- slide2(
    .x, .y, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' @rdname slide2
#' @inheritParams purrr::pmap
#' @export
pslide <- function(.l, .f, ..., .size = 1, .fill = NA, .partial = FALSE) {
  if (is.data.frame(.l)) {
    .l <- as.list(.l)
  }
  lst <- pslider(!!! .l, .size = .size, .fill = .fill, .partial = .partial)
  out <- purrr::pmap(lst, .f, ...)
  if (.partial) return(out)
  pad_slide(out, .size, .fill)
}

#' @evalRd paste0('\\alias{pslide_', c("lgl", "chr", "int", "dbl"), '}')
#' @exportPattern ^pslide_
for(type in c("lgl", "chr", "int", "dbl")){
  assign(
    paste0("pslide_", type),
    replace_fn_names(pslide, list(pmap = sym(paste0("pmap_", type))))
  )
}

#' @rdname slide2
#' @export
pslide_dfr <- function(
  .l, .f, ..., .size = 1, .fill = NA, .partial = FALSE, .id = NULL
) {
  out <- pslide(
    .l, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial
  )
  bind_df(out, .size, .fill, .id = .id)
}

#' @rdname slide2
#' @export
#' @examples
#' ## window over 2 months
#' pedestrian %>% 
#'   filter(Sensor == "Southern Cross Station") %>% 
#'   index_by(yrmth = yearmonth(Date_Time)) %>% 
#'   nest(-yrmth) %>% 
#'   mutate(ma = slide_dbl(data, ~ mean(do.call(rbind, .)$Count), .size = 2))
#' # row-oriented workflow
#' \dontrun{
#' my_diag <- function(...) {
#'   data <- list(...)
#'   fit <- lm(Count ~ Time, data = data)
#'   tibble::tibble(fitted = fitted(fit), resid = residuals(fit))
#' }
#' pedestrian %>%
#'   filter(Date <= as.Date("2015-01-31")) %>%
#'   nest(-Sensor) %>%
#'   mutate(diag = purrr::map(data, ~ pslide_dfr(., my_diag, .size = 48)))
#' }
pslide_dfc <- function(.l, .f, ..., .size = 1, .fill = NA, .partial = FALSE) {
  out <- pslide(
    .l, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' Splits the input to a list according to the rolling window size.
#'
#' @param x An objects to be split.
#' @param ... Multiple objects to be split in parallel.
#' @param .partial if `TRUE`, split to partial set (`FALSE` ignores specified 
#' `.fill`).
#' @inheritParams slide
#' @rdname slider
#' @export
#' @examples
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' lst <- list(x = x, y = y, z = z)
#' df <- as.data.frame(lst)
#'
#' slider(x, .size = 2)
#' slider(lst, .size = 2)
#' pslider(list(x, y), list(y))
#' slider(df, .size = 2)
#' pslider(df, df, .size = 2)
slider <- function(.x, .size = 1, .fill = NA, .partial = FALSE) {
  if (is.data.frame(.x)) .x <- as.list(.x)
  slider_base(.x, .size, .fill = .fill, .partial)
}

#' @rdname slider
#' @export
pslider <- function(..., .size = 1, .fill = NA, .partial = FALSE) { # parallel sliding
  lst <- recycle(list2(...))
  df_lgl <- purrr::map_lgl(lst, is.data.frame)
  if (any(df_lgl)) {
    lst[df_lgl] <- purrr::map(lst[df_lgl], as.list)
  }
  purrr::map(lst, function(x) slider_base(x, .size, .fill = .fill, .partial))
}

slider_base <- function(x, .size = 1, .fill = NA, .partial = FALSE) {
  bad_window_function(.size)
  len_x <- NROW(x)
  abs_size <- abs(.size)
  if (abs_size > len_x) {
    abort(sprintf(
      "`abs(.size)` (%s) must not be larger than the length (%s) of the input.",
      abs_size, len_x
    ))
  }
  sign <- sign(.size)
  if (.partial) {
    lst_idx <- seq_len(len_x) - sign * (abs_size - 1)
    if (sign < 0) lst_idx <- rev(lst_idx)
    return(purrr::map(lst_idx, function(idx) {
      idx <- idx:(idx + sign * (abs_size - 1))
      c(rep(.fill, sum(idx <= 0 | idx > len_x)), x[idx[idx > 0 & idx <= len_x]])
    }))
  }
  lst_idx <- seq_len(len_x - abs_size + 1)
  if (sign < 0) lst_idx <- rev(lst_idx) + 1
  purrr::map(lst_idx, function(idx) x[idx:(idx + sign * (abs_size - 1))])
}

bad_window_function <- function(.size) {
  if (!is_integerish(.size, n = 1)) {
    abort("`.size` must be an integer.")
  }
  if (.size == 0) {
    abort("`.size` must not be 0.")
  }
}

recycle <- function(x) {
  if (has_length(x, 0)) {
    return(x)
  }
  len <- purrr::map_int(x, length)
  max_len <- max(len)
  len1 <- len == 1
  check <- !len1 & len != max_len
  if (any(check)) {
    bad <- which(check)[1]
    abort(sprintf(
      "Element %s has length %s, not 1 or %s.", bad, len[bad], max_len
    ))
  }
  if (sum(len1) == 0) return(x)
  rep_idx <- which(len1)
  x[len1] <- lapply(x[rep_idx], rep, max_len)
  x
}

pad_slide <- function(x, .size = 1, .fill = NA) {
  if (is_null(.fill)) return(x) 
  c(rep_len(.fill, abs(.size) - 1), x)
}

bind_df <- function(x, .size, .fill = NA, .id = NULL, byrow = TRUE) {
  abs_size <- abs(.size)
  if (abs_size < 2) {
    if (byrow) return(dplyr::bind_rows(x, .id = .id))
    return(dplyr::bind_cols(x))
  }
  lst <- new_list_along(x[[abs_size]])
  lst[] <- .fill
  if (byrow) 
    return(dplyr::bind_rows(lst, !!! x[-seq_len(abs_size - 1)], .id = .id))
  dplyr::bind_cols(lst, !!! x[-seq_len(abs_size - 1)])
}
