# nocov start
replace_fn_names <- function(fn, replace = list()){
  rec_fn <- function(cl) {
    if (!rlang::is_call(cl)) {
      return(cl)
    }
    if (any(repl_fn <- names(replace) %in% rlang::call_name(cl))) {
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
#' @param .align Align index at the "**r**ight", "**c**entre"/"center", or "**l**eft"
#' of the window. If `.size` is even for center alignment, "centre-right" & "centre-left"
#' is needed.
#' @param .bind If `.x` is a list, should `.x` be combined before applying `.f`?
#' If `.x` is a list of data frames, row binding is carried out.
#'
#' @rdname slide
#' @export
#' @seealso
#' * [slide2], [pslide]
#' * [tile] for tiling window without overlapping observations
#' * [stretch] for expanding more observations
#' @details The `slide()` function attempts to tackle more general problems using
#' the purrr-like syntax. For some specialist functions like `mean` and `sum`,
#' you may like to check out for **RcppRoll** for faster performance.
#'
#' `slide()` is intended to work with list (and column-wise data frame). To
#' perform row-wise sliding window on data frame, please check out [pslide()].
#'
#' @examples
#' x <- 1:5
#' lst <- list(x = x, y = 6:10, z = 11:15)
#' slide_dbl(x, mean, .size = 2)
#' slide_dbl(x, mean, .size = 2, align = "center")
#' slide_lgl(x, ~ mean(.) > 2, .size = 2)
#' slide(lst, ~ ., .size = 2)
slide <- function(
  .x, .f, ..., .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) {
  lst_x <- slider(
    .x, .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
  )
  out <- purrr::map(lst_x, .f, ...)
  if (.partial) return(out)
  pad_slide(out, .size, .fill, .align)
}

#' @evalRd paste0('\\alias{slide_', c("lgl", "chr", "int", "dbl"), '}')
#' @name slide
#' @rdname slide
#' @exportPattern ^slide_
for(type in c("lgl", "chr", "int", "dbl")){
  assign(
    paste0("slide_", type),
    replace_fn_names(slide, list(map = rlang::sym(paste0("map_", type))))
  )
}

#' @rdname slide
#' @export
slide_dfr <- function(
  .x, .f, ..., .size = 1, .fill = NA, .partial = FALSE, .align = "right", 
  .bind = FALSE, .id = NULL
) {
  out <- slide(
    .x, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, .id = .id)
}

#' @rdname slide
#' @export
slide_dfc <- function(
  .x, .f, ..., .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) {
  out <- slide(
    .x, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
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
#'
#' ###
#' # row-wise sliding over data frame
#' ###
#'
#' my_df <- data.frame(
#'   group = rep(letters[1:2], each = 8),
#'   x = c(1:8, 8:1),
#'   y = 2 * c(1:8, 8:1) + rnorm(16),
#'   date = rep(as.Date("2016-06-01") + 0:7, 2)
#' )
#' 
#' slope <- function(...) {
#'   data <- list(...)
#'   fm <- lm(y ~ x, data = data)
#'   coef(fm)[[2]]
#' }
#' 
#' my_df %>% 
#'   nest(-group) %>% 
#'   mutate(slope = purrr::map(data, ~ pslide_dbl(., slope, .size = 2))) %>% 
#'   unnest()
slide2 <- function(
  .x, .y, .f, ..., .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) {
  lst <- pslider(
    .x, .y, .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
  )
  out <- purrr::map2(lst[[1]], lst[[2]], .f = .f, ...)
  if (.partial) return(out)
  pad_slide(out, .size, .fill, .align)
}

#' @evalRd paste0('\\alias{slide2_', c("lgl", "chr", "int", "dbl"), '}')
#' @name slide2
#' @rdname slide2
#' @exportPattern ^slide2_
for(type in c("lgl", "chr", "int", "dbl")){
  assign(
    paste0("slide2_", type),
    replace_fn_names(slide2, list(map2 = rlang::sym(paste0("map2_", type))))
  )
}

#' @rdname slide2
#' @export
slide2_dfr <- function(
  .x, .y, .f, ..., .size = 1, .fill = NA, .partial = FALSE, .align = "right",
  .bind = FALSE, .id = NULL
) {
  out <- slide2(
    .x, .y, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, .id = .id)
}

#' @rdname slide2
#' @export
slide2_dfc <- function(
  .x, .y, .f, ..., .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) {
  out <- slide2(
    .x, .y, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' @rdname slide2
#' @inheritParams purrr::pmap
#' @export
pslide <- function(
  .l, .f, ..., .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) {
  lst <- pslider(
    !!! .l, .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
  )
  out <- purrr::pmap(lst, .f, ...)
  if (.partial) return(out)
  pad_slide(out, .size, .fill, .align)
}

#' @evalRd paste0('\\alias{pslide_', c("lgl", "chr", "int", "dbl"), '}')
#' @name pslide
#' @rdname slide2
#' @exportPattern ^pslide_
for(type in c("lgl", "chr", "int", "dbl")){
  assign(
    paste0("pslide_", type),
    replace_fn_names(pslide, list(pmap = rlang::sym(paste0("pmap_", type))))
  )
}

#' @rdname slide2
#' @export
pslide_dfr <- function(
  .l, .f, ..., .size = 1, .fill = NA, .partial = FALSE, .align = "right",
  .bind = FALSE, .id = NULL
) {
  out <- pslide(
    .l, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
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
#'   mutate(ma = slide_dbl(data, ~ mean(.$Count), .size = 2, .bind = TRUE))
#' # row-oriented workflow
#' \dontrun{
#' my_diag <- function(...) {
#'   data <- list(...)
#'   fit <- lm(Count ~ Time, data = data)
#'   tibble(fitted = fitted(fit), resid = residuals(fit))
#' }
#' pedestrian %>%
#'   filter(Date <= as.Date("2015-01-31")) %>%
#'   nest(-Sensor) %>%
#'   mutate(diag = purrr::map(data, ~ pslide_dfr(., my_diag, .size = 48)))
#' }
pslide_dfc <- function(
  .l, .f, ..., .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) {
  out <- pslide(
    .l, .f = .f, ..., 
    .size = .size, .fill = .fill, .partial = .partial, 
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' Splits the input to a list according to the rolling window size.
#'
#' @param x An objects to be split.
#' @param ... Multiple objects to be split in parallel.
#' @param .partial if `TRUE`, split to partial set (`FALSE` ignores specified 
#' `.fill` and `.align`).
#' @param .bind If `.x` is a list or data frame, the input will be flattened
#' to a list of data frames.
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
slider <- function(
  .x, .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) {
  bad_window_function(.size)
  abort_not_lst(.x, .bind = .bind)
  if (is.data.frame(.x)) .x <- as.list(.x)
  len_x <- NROW(.x)
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
    out <- purrr::map(lst_idx, function(idx) {
      idx <- idx:(idx + sign * (abs_size - 1))
      size <- sum(idx <= 0 | idx > len_x) + 1
      pad_slide(.x[idx[idx > 0 & idx <= len_x]], size, .fill, .align)
    })
    if (.bind) return(bind_lst(out)) else return(out)
  }
  lst_idx <- seq_len(len_x - abs_size + 1)
  if (sign < 0) lst_idx <- rev(lst_idx) + 1
  out <- purrr::map(lst_idx, function(idx) .x[idx:(idx + sign * (abs_size - 1))])
  if (.bind) bind_lst(out) else out
}

#' @rdname slider
#' @export
pslider <- function(
  ..., .size = 1, .fill = NA, .partial = FALSE, 
  .align = "right", .bind = FALSE
) { # parallel sliding
  lst <- recycle(list2(...))
  purrr::map(lst, 
    function(x) slider(x, .size, .fill = .fill, .partial, .align, .bind)
  )
}

bind_lst <- function(x) {
  type_elements <- flatten_chr(purrr::modify_depth(x, 2, ~ typeof(.)[1]))
  if (all(type_elements == "list")) {
    lapply(x, dplyr::bind_rows)
  } else {
    lapply(x, dplyr::combine)
  }
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

pad_slide <- function(x, .size = 1, .fill = NA, .align = "right") {
  align <- match.arg(.align,
    c("right", "c", "center", "centre", "left",
      "cr", "center-right", "centre-right", 
      "cl", "center-left", "centre-left"
    )
  )
  check_valid_window(.size, align)
  if (is_null(.fill)) return(x) 
  fill_size <- abs(.size) - 1
  if (align == "right") {
    c(rep(.fill, fill_size), x)
  } else if (align %in% c("c", "center", "centre", "cl", "center-left", "centre-left")) {
    lsize <- floor(fill_size / 2)
    c(rep(.fill, lsize), x, rep(.fill, fill_size - lsize))
  } else if (align %in% c("cr", "center-right", "centre-right")) {
    lsize <- ceiling(fill_size / 2)
    c(rep(.fill, lsize), x, rep(.fill, fill_size - lsize))
  } else {
    c(x, rep(.fill, fill_size)) # "left"
  }
}

bind_df <- function(x, .size, .fill = NA, .id = NULL, byrow = TRUE) {
  abs_size <- abs(.size)
  if (abs_size < 2) {
    if (byrow) return(dplyr::bind_rows(x, .id = .id))
    return(dplyr::bind_cols(x))
  }
  lst <- new_list_along(x[[abs_size]])
  lst[] <- .fill
  if (byrow) {
    dplyr::bind_rows(lst, !!! x[-seq_len(abs_size - 1)], .id = .id)
  } else {
    dplyr::bind_cols(lst, !!! x[-seq_len(abs_size - 1)])
  }
}

check_valid_window <- function(.size, .align) {
  if (is_even(.size) && .align %in% c("c", "centre", "center")) {
    abort(sprintf(
      "Can't use `.align = %s` for even window `.size`. Please use `.align = 'center-left'` or `.align = 'center-right'`.",
      .align
    ))
  }
}

abort_not_lst <- function (.x, .bind = FALSE) {
  if (!is.list(.x) && .bind) {
    abort(sprintf("`.bind = TRUE` only accepts list, not %s.", typeof(.x)))
  }
}
