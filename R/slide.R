# nocov start
replace_fn_names <- function(fn, replace = list(), ns = NULL) {
  rec_fn <- function(cl) {
    if (!is_call(cl)) {
      return(cl)
    }
    args_lst <- lapply(as.list(cl[-1]), rec_fn)
    if (any(repl_fn <- names(replace) %in% call_name(cl))) {
      cl[[1]] <- replace[[repl_fn]]
      cl <- call2(cl[[1]], !!!args_lst, .ns = ns)
    } else {
      cl <- call2(cl[[1]], !!!args_lst)
    }
  }
  body(fn) <- rec_fn(body(fn))
  fn
}
# nocov end

#' Sliding window calculation
#'
#' @description
#' \lifecycle{stable}
#'
#' Rolling window with overlapping observations:
#' * `slide()` always returns a list.
#' * `slide_lgl()`, `slide_int()`, `slide_dbl()`, `slide_chr()` use the same
#' arguments as `slide()`, but return vectors of the corresponding type.
#' * `slide_dfr()` & `slide_dfc()` return data frames using row-binding & column-binding.
#'
#' @param .x An object to slide over.
#' @inheritParams purrr::map
#' @param .size An integer for window size. If positive, moving forward from left
#' to right; if negative, moving backward (from right to left).
#' @param .step A positive integer for calculating at every specified step
#' instead of every single step.
#' @param .fill A value to fill at the left/center/right of the data range depending
#' on `.align` (`NA` by default).  `NULL` means no filling.
#' @param .partial if `TRUE`, partial sliding.
#' @param .align Align index at the "**r**ight", "**c**entre"/"center", or "**l**eft"
#' of the window. If `.size` is even for center alignment, "centre-right" & "centre-left"
#' is needed.
#' @param .bind If `.x` is a list, should `.x` be combined before applying `.f`?
#' If `.x` is a list of data frames, row binding is carried out.
#'
#' @return if `.fill != NULL`, it always returns the same length as input.
#' @rdname slide
#' @export
#' @family sliding window functions
#' @seealso
#' * [future_slide] for parallel processing
#' * [tile] for tiling window without overlapping observations
#' * [stretch] for expanding more observations
#' @details The `slide()` function attempts to tackle more general problems using
#' the purrr-like syntax. For some specialist functions like `mean` and `sum`,
#' you may like to check out for **RcppRoll** for faster performance.
#'
#' `slide()` is intended to work with list (and column-wise data frame). To
#' perform row-wise sliding window on data frame, please check out [pslide()].
#'
#' * `.partial = TRUE` allows for partial sliding. Window contains observations
#' outside of the vector will be treated as value of `.fill`, which will be passed to `.f`.
#' * `.partial = FALSE` restricts calculations to be done on complete sliding windows.
#' Window contains observations outside of the vector will return the value `.fill`.
#'
#' @examples
#' x <- 1:5
#' lst <- list(x = x, y = 6:10, z = 11:15)
#' slide_dbl(x, mean, .size = 2)
#' slide_dbl(x, mean, .size = 2, align = "center")
#' slide_lgl(x, ~ mean(.) > 2, .size = 2)
#' slide(lst, ~., .size = 2)
slide <- function(.x, .f, ..., .size = 1, .step = 1, .fill = NA,
                  .partial = FALSE, .align = "right", .bind = FALSE) {
  if (.partial) {
    lst_x <- partial_slider(
      .x,
      .size = .size, .step = .step, .fill = .fill,
      .align = .align, .bind = .bind
    )
    .size <- 1L
  } else {
    lst_x <- slider(.x, .size = .size, .step = .step, .bind = .bind)
  }
  out <- map(lst_x, .f, ...)
  pad_slide(out, .size, .step = .step, .fill, .align, expect_length = nrow2(.x))
}

#' @evalRd paste0('\\alias{slide_', c("lgl", "chr", "int", "dbl"), '}')
#' @name slide
#' @rdname slide
#' @exportPattern ^slide_
for (type in c("lgl", "chr", "int", "dbl")) {
  assign(
    paste0("slide_", type),
    replace_fn_names(slide, list(map = paste0("map_", type)))
  )
}

#' @rdname slide
#' @export
slide_dfr <- function(.x, .f, ..., .size = 1, .step = 1, .fill = NA,
                      .partial = FALSE, .align = "right", .bind = FALSE,
                      .id = NULL) {
  out <- slide(
    .x,
    .f = .f, ...,
    .size = .size, .step = .step, .fill = .fill, .partial = .partial,
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, .id = .id)
}

#' @rdname slide
#' @export
slide_dfc <- function(.x, .f, ..., .size = 1, .step = 1, .fill = NA,
                      .partial = FALSE, .align = "right", .bind = FALSE) {
  out <- slide(
    .x,
    .f = .f, ...,
    .size = .size, .step = .step, .fill = .fill, .partial = .partial,
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' Sliding window calculation over multiple inputs simultaneously
#'
#' @description
#' \lifecycle{stable}
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
#' @family sliding window functions
#' @seealso
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
#' slide2(lst, lst, ~., .size = 2)
#' slide2(df, df, ~., .size = 2)
#' pslide(lst, ~., .size = 1)
#' pslide(list(lst, lst), ~., .size = 2)
#'
#' ###
#' # row-wise sliding over data frame
#' ###
#'
#' if (!requireNamespace("tidyr", quietly = TRUE)) {
#'   stop("Please install the 'tidyr' package to run these following examples.")
#' }
#' library(tidyr)
#' library(dplyr)
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
#'   group_by(group) %>%
#'   nest() %>%
#'   mutate(slope = purrr::map(data, ~ pslide_dbl(., slope, .size = 2))) %>%
#'   unnest(slope)
slide2 <- function(.x, .y, .f, ..., .size = 1, .step = 1, .fill = NA,
                   .partial = FALSE, .align = "right", .bind = FALSE) {
  if (.partial) {
    lst <- partial_pslider(
      .x, .y,
      .size = .size, .step = .step, .fill = .fill,
      .align = .align, .bind = .bind
    )
    .size <- 1L
  } else {
    lst <- pslider(.x, .y, .size = .size, .step = .step, .bind = .bind)
  }
  out <- map2(lst[[1]], lst[[2]], .f = .f, ...)
  pad_slide(out, .size, .step, .fill, .align,
    expect_length = nrow2(recycle(list(.x, .y))[[1]])
  )
}

#' @evalRd paste0('\\alias{slide2_', c("lgl", "chr", "int", "dbl"), '}')
#' @name slide2
#' @rdname slide2
#' @exportPattern ^slide2_
for (type in c("lgl", "chr", "int", "dbl")) {
  assign(
    paste0("slide2_", type),
    replace_fn_names(slide2, list(map2 = paste0("map2_", type)))
  )
}

#' @rdname slide2
#' @export
slide2_dfr <- function(.x, .y, .f, ..., .size = 1, .step = 1, .fill = NA,
                       .partial = FALSE, .align = "right", .bind = FALSE,
                       .id = NULL) {
  out <- slide2(
    .x, .y,
    .f = .f, ...,
    .size = .size, .step = .step, .fill = .fill, .partial = .partial,
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, .id = .id)
}

#' @rdname slide2
#' @export
slide2_dfc <- function(.x, .y, .f, ..., .size = 1, .step = 1, .fill = NA,
                       .partial = FALSE, .align = "right", .bind = FALSE) {
  out <- slide2(
    .x, .y,
    .f = .f, ...,
    .size = .size, .step = .step, .fill = .fill, .partial = .partial,
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' @rdname slide2
#' @inheritParams purrr::pmap
#' @export
pslide <- function(.l, .f, ..., .size = 1, .step = 1, .fill = NA,
                   .partial = FALSE, .align = "right", .bind = FALSE) {
  if (.partial) {
    lst <- partial_pslider(
      !!!.l,
      .size = .size, .step = .step, .fill = .fill,
      .align = .align, .bind = .bind
    )
    .size <- 1L
  } else {
    lst <- pslider(!!!.l, .size = .size, .step = .step, .bind = .bind)
  }
  out <- pmap(lst, .f, ...)
  pad_slide(out, .size, .step, .fill, .align,
    expect_length = nrow2(recycle(.l)[[1]])
  )
}

#' @evalRd paste0('\\alias{pslide_', c("lgl", "chr", "int", "dbl"), '}')
#' @name pslide
#' @rdname slide2
#' @exportPattern ^pslide_
for (type in c("lgl", "chr", "int", "dbl")) {
  assign(
    paste0("pslide_", type),
    replace_fn_names(pslide, list(pmap = paste0("pmap_", type)))
  )
}

#' @rdname slide2
#' @export
pslide_dfr <- function(.l, .f, ..., .size = 1, .step = 1, .fill = NA,
                       .partial = FALSE, .align = "right", .bind = FALSE,
                       .id = NULL) {
  out <- pslide(
    .l,
    .f = .f, ...,
    .size = .size, .step = .step, .fill = .fill, .partial = .partial,
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
#'   nest(data = -yrmth) %>%
#'   ungroup() %>% 
#'   mutate(ma = slide_dbl(data, ~ mean(.$Count), .size = 2, .bind = TRUE))
#' # row-oriented workflow
#' \dontrun{
#' my_diag <- function(...) {
#'   data <- list(...)
#'   fit <- lm(Count ~ Time, data = data)
#'   tibble(fitted = fitted(fit), resid = residuals(fit))
#' }
#' pedestrian %>%
#'   filter_index("2015-01") %>%
#'   group_by_key() %>%
#'   nest() %>%
#'   mutate(diag = purrr::map(data, ~ pslide_dfr(., my_diag, .size = 48)))
#' }
pslide_dfc <- function(.l, .f, ..., .size = 1, .step = 1, .fill = NA,
                       .partial = FALSE, .align = "right", .bind = FALSE) {
  out <- pslide(
    .l,
    .f = .f, ...,
    .size = .size, .step = .step, .fill = .fill, .partial = .partial,
    .align = .align, .bind = .bind
  )
  bind_df(out, .size, .fill, byrow = FALSE)
}

#' Splits the input to a list according to the rolling window size.
#'
#' @param .x An objects to be split.
#' @param ... Multiple objects to be split in parallel.
#' @param .bind If `.x` is a list or data frame, the input will be flattened
#' to a list of data frames.
#' @inheritParams slide
#' @rdname slider
#' @seealso [partial_slider], [partial_pslider] for partial sliding
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
slider <- function(.x, .size = 1, .step = 1, .bind = FALSE) {
  .x <- check_slider_input(.x, .size = .size, .step = .step, .bind = .bind)
  len_x <- NROW(.x)
  abs_size <- abs(.size)
  if (abs_size > len_x) {
    abort(sprintf(slider_msg(), abs_size, len_x))
  }
  sign <- sign(.size)
  if (sign > 0) {
    lst_idx <- seq.int(1L, len_x - abs_size + 1, by = .step)
  } else {
    lst_idx <- seq.int(len_x, abs_size, by = sign * .step)
  }
  out <- map(lst_idx, function(idx) .x[idx:(idx + sign * (abs_size - 1))])
  if (.bind) bind_lst(out) else out
}

#' @rdname slider
#' @export
pslider <- function(..., .size = 1, .step = 1, .bind = FALSE) {
  # parallel sliding
  lst <- recycle(list2(...))
  map(lst, function(x) slider(x, .size = .size, .step = .step, .bind))
}

#' Partially splits the input to a list according to the rolling window size.
#'
#' @inheritParams slide
#' @rdname partial-slider
#' @export
#' @examples
#' x <- c(1, NA_integer_, 3:5)
#' slider(x, .size = 3)
#' partial_slider(x, .size = 3)
partial_slider <- function(.x, .size = 1, .step = 1, .fill = NA,
                           .align = "right", .bind = FALSE) {
  check_valid_window(.size, .align)
  .x <- pad_slide(.x,
    .size = .size, .step = 1L, .fill = .fill,
    .align = .align, expect_length = NROW(.x) + abs(.size) - 1L,
    .partial = TRUE
  )
  slider(.x, .size = .size, .step = .step, .bind = .bind)
}

#' @rdname partial-slider
#' @export
partial_pslider <- function(..., .size = 1, .step = 1, .fill = NA,
                            .align = "right", .bind = FALSE) {
  lst <- recycle(list2(...))
  map(lst, function(x) partial_slider(x, .size, .step, .fill, .align, .bind))
}

#' Perform sliding windows on a tsibble by row
#'
#' \lifecycle{questioning}
#'
#' @param .x A tsibble.
#' @param .size A positive integer for window size.
#' @inheritParams slide
#' @param .id A character naming the new column `.id` containing the partition.
#'
#' @section Rolling tsibble:
#' `slide_tsibble()`, `tile_tsibble()`, and `stretch_tsibble()` provide fast
#' and shorthand for rolling over a tsibble by observations. That said, if the
#' supplied tsibble has time gaps, these rolling helpers will ignore those gaps
#' and proceed.
#'
#' They are useful for preparing the tsibble for time series cross validation.
#' They all return a tsibble including a new column `.id` as part of the key. The
#' output dimension will increase considerably with `slide_tsibble()` and
#' `stretch_tsibble()`, which is likely to run out of memory when the data is
#' large. Alternatively, you could construct cross validation using `pslide()`
#' and `pstretch()` to avoid the memory issue.
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
#'   slide_tsibble(.size = 2)
slide_tsibble <- function(.x, .size = 1, .step = 1, .id = ".id") {
  lst_indices <- map(key_rows(.x), slider, .size = .size, .step = .step)
  roll_tsibble(.x, indices = lst_indices, .id = .id)
}

# fast_slider <- function(.x, .size = 1, .step = 1) {
#   len_x <- NROW(.x)
#   lst_idx <- seq.int(1L, len_x - .size + 1, by = .step)
#   len_idx <- length(lst_idx)
#   idx <- kronecker(lst_idx, matrix(seq_len(.size) - 1, ncol = .size), "+")
#   .x[idx, ]
#   # id_indices <- rep.int(seq_len(len_idx), .size)
#   # list(indices = .x[idx], id_indices = id_indices)
# }

roll_tsibble <- function(.x, indices, .id = ".id") {
  if (.id %in% names(.x)) {
    abort(sprintf("Can't overwrite existing column `%s`.", .id))
  }
  tbl <- as_tibble(ungroup(.x))
  row_indices <- unlist(indices, use.names = FALSE)
  id_indices <-
    unlist(map(
      indices,
      ~ imap(.x, ~ rep.int(.y, length(.x)))
    ), use.names = FALSE)
  res <-
    group_by(
      mutate(tbl[row_indices, ], !!.id := id_indices),
      !!!groups(.x)
    )
  new_key <- c(.id, key_vars(.x))
  build_tsibble(res,
    key = !!new_key, index = !!index(.x), index2 = !!index2(.x),
    interval = interval(.x), validate = FALSE
  )
}

check_slider_input <- function(.x, .size = 1, .step = 1, .bind = FALSE) {
  bad_window_function(.size)
  bad_step_function(.step)
  abort_not_lst(.x, .bind = .bind)
  if (is.data.frame(.x)) .x <- as.list(.x)
  .x
}

bind_lst <- function(x) {
  type_elements <- flatten_chr(purrr::modify_depth(x, 2, ~ typeof(.)[1]))
  if (all(type_elements == "list")) {
    lapply(x, bind_rows)
  } else {
    lapply(x, combine)
  }
}

recycle <- function(x) {
  if (has_length(x, 0)) {
    return(x)
  }

  len <- map_int(x, length)
  max_len <- max(len)
  len1 <- len == 1
  check <- !len1 & len != max_len
  if (any(check)) {
    bad <- which(check)[1]
    abort(sprintf(
      "Element %s has length %s, not 1 or %s.", bad, len[bad], max_len
    ))
  }
  if (sum(len1) == 0) {
    return(x)
  }
  rep_idx <- which(len1)
  x[len1] <- lapply(x[rep_idx], rep, max_len)
  x
}

pad_slide <- function(x, .size = 1, .step = 1, .fill = NA, .align = "right",
                      expect_length = NULL, .partial = FALSE) {
  .align <- match.arg(
    .align,
    c(
      "right", "c", "center", "centre", "left",
      "cr", "center-right", "centre-right",
      "cl", "center-left", "centre-left"
    )
  )
  check_valid_window(.size, .align)
  if (is_null(.fill)) {
    return(x)
  }

  cl <- c("c", "center", "centre", "cl", "center-left", "centre-left")
  cr <- c("cr", "center-right", "centre-right")
  if (!.partial && .size < 0) {
    if (.align == "right") {
      .align <- "left"
    } else if (.align == "left") {
      .align <- "right"
    } else if (.align %in% cl) {
      .align <- cr
    } else {
      .align <- cl
    }
  }

  len_x <- length(x)
  fill_size <- abs(.size) - 1
  if (.step == 1) {
    if (.align == "right") {
      c(rep(.fill, fill_size), x)
    } else if (.align == "left") {
      c(x, rep(.fill, fill_size))
    } else if (.align %in% cl) {
      lsize <- floor(fill_size / 2)
      c(rep(.fill, lsize), x, rep(.fill, fill_size - lsize))
    } else {
      lsize <- ceiling(fill_size / 2)
      c(rep(.fill, lsize), x, rep(.fill, fill_size - lsize))
    }
  } else {
    seq_x <- seq_len(len_x)
    rep_idx <- rep.int(len_x + 1, len_x * (.step - 1))
    null_idx <- matrix(rep_idx, nrow = .step - 1)
    idx <- as.integer(rbind(seq_x, null_idx, deparse.level = 0))
    res <- x[idx]
    res[!(idx %in% seq_x)] <- .fill
    if (.align == "right") {
      res <- c(rep(.fill, fill_size), res)
    } else if (.align == "left") {
      res <- c(res, rep(.fill, fill_size))
    } else if (.align %in% cl) {
      lsize <- floor(fill_size / 2)
      res <- c(rep(.fill, lsize), res, rep(.fill, fill_size - lsize))
    } else {
      lsize <- ceiling(fill_size / 2)
      res <- c(rep(.fill, lsize), res, rep(.fill, fill_size - lsize))
    }
    res[seq_len(expect_length)]
  }
}

bind_df <- function(x, .size, .fill = NA, .id = NULL, byrow = TRUE) {
  abs_size <- abs(.size)
  if (abs_size < 2) {
    if (byrow) {
      return(bind_rows(!!!x, .id = .id))
    } else {
      return(bind_cols(!!!x))
    }
  }
  lst <- rep_named(names(x[[abs_size]]), list(.fill))
  if (byrow) {
    bind_rows(lst, !!!x[-seq_len(abs_size - 1)], .id = .id)
  } else {
    bind_cols(lst, !!!x[-seq_len(abs_size - 1)])
  }
}

slider_msg <- function() {
  "`abs(.size)` (%s) must not be larger than the length (%s) of the input."
}

#' Sliding window in parallel
#'
#' Multiprocessing equivalents of [slide()], [tile()], [stretch()] prefixed by `future_`.
#' * Variants for corresponding types: `future_*_lgl()`, `future_*_int()`,
#' `future_*_dbl()`, `future_*_chr()`, `future_*_dfr()`, `future_*_dfc()`.
#' * Extra arguments `.progress` and `.options` for enabling progress bar and the
#' future specific options to use with the workers.
#'
#' @details
#' It requires the package **furrr** to be installed. Please refer to [furrr](https://davisvaughan.github.io/furrr/) for performance and detailed usage.
#' @evalRd {suffix <- c("lgl", "chr", "int", "dbl", "dfr", "dfc"); c(paste0('\\alias{future_', c("slide", "slide2", "pslide"), '}'), paste0('\\alias{future_slide_', suffix, '}'), paste0('\\alias{future_slide2_', suffix, '}'), paste0('\\alias{future_pslide_', suffix, '}'))}
#' @name future_slide()
#' @rdname future-slide
#' @exportPattern ^future_
#' @examples
#' if (!requireNamespace("furrr", quietly = TRUE)) {
#'   stop("Please install the furrr package to run these following examples.")
#' }
#' \dontrun{
#' library(furrr)
#' plan(multiprocess)
#' my_diag <- function(...) {
#'   data <- list(...)
#'   fit <- lm(Count ~ Time, data = data)
#'   tibble(fitted = fitted(fit), resid = residuals(fit))
#' }
#' pedestrian %>%
#'   group_by_key() %>%
#'   nest() %>%
#'   mutate(diag = future_map(data, ~ future_pslide_dfr(., my_diag, .size = 48)))
#' }
#' # nocov start
assign("future_slide", replace_fn_names(slide, list(map = "future_map"), ns = "furrr"))
assign("future_slide2", replace_fn_names(slide2, list(map2 = "future_map2"), ns = "furrr"))
assign("future_pslide", replace_fn_names(pslide, list(pmap = "future_pmap"), ns = "furrr"))
assign("future_slide_dfr", replace_fn_names(slide_dfr, list(slide = "future_slide")))
assign("future_slide2_dfr", replace_fn_names(slide2_dfr, list(slide2 = "future_slide2")))
assign("future_pslide_dfr", replace_fn_names(pslide_dfr, list(pslide = "future_pslide")))
assign("future_slide_dfc", replace_fn_names(slide_dfc, list(slide = "future_slide")))
assign("future_slide2_dfc", replace_fn_names(slide2_dfc, list(slide2 = "future_slide2")))
assign("future_pslide_dfc", replace_fn_names(pslide_dfc, list(pslide = "future_pslide")))
for (type in c("lgl", "chr", "int", "dbl")) {
  assign(
    paste0("future_slide_", type),
    replace_fn_names(slide, list(map = paste0("future_map_", type)), ns = "furrr")
  )
  assign(
    paste0("future_slide2_", type),
    replace_fn_names(slide2, list(map2 = paste0("future_map2_", type)), ns = "furrr")
  )
  assign(
    paste0("future_pslide_", type),
    replace_fn_names(pslide, list(pmap = paste0("future_pmap_", type)), ns = "furrr")
  )
}
# nocov end
