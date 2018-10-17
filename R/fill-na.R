globalVariables(".")

#' Turn implicit missing values into explicit missing values
#'
#' @param .data A data frame.
#' @param ... A set of name-value pairs. The values will replace existing explicit
#' missing values by variable, otherwise `NA`. The replacement values must be of
#' the same type as the original one.
#'
#' @seealso [count_gaps], [tidyr::fill], [tidyr::replace_na]
#' @rdname fill-na
#' @export
fill_na <- function(.data, ...) {
  UseMethod("fill_na")
}

#' @export
fill_na.data.frame <- function(.data, ...) {
  abort("Do you need `tidyr::complete()` for a `tbl_df`/`data.frame`?")
}

#' @rdname fill-na
#' @param .full `FALSE` to insert `NA` for each key within its own period. `TRUE`
#' to fill `NA` over the entire time span of the data (a.k.a. fully balanced panel).
#'
#' @examples
#' harvest <- tsibble(
#'   year = c(2010, 2011, 2013, 2011, 2012, 2014),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = id(fruit), index = year
#' )
#'
#' # leave NA as is ----
#' fill_na(harvest, .full = TRUE)
#' full_harvest <- fill_na(harvest, .full = FALSE)
#' full_harvest
#'
#' # use fill() to fill `NA` by previous/next entry
#' full_harvest %>% 
#'   group_by(fruit) %>% 
#'   tidyr::fill(kilo, .direction = "down")
#'
#' # replace NA with a specific value ----
#' harvest %>%
#'   fill_na(kilo = 0L)
#'
#' # replace NA using a function by variable ----
#' harvest %>%
#'   fill_na(kilo = sum(kilo))
#'
#' # replace NA using a function for each group ----
#' harvest %>%
#'   group_by(fruit) %>%
#'   fill_na(kilo = sum(kilo))
#'
#' # replace NA ----
#' pedestrian %>%
#'   group_by(Sensor) %>%
#'   fill_na(Count = as.integer(median(Count)))
#' @export
fill_na.tbl_ts <- function(.data, ..., .full = FALSE) {
  not_regular(.data)
  unknown_interval(int <- interval(.data))
  idx <- index(.data)
  idx_chr <- as_string(idx)
  key <- key(.data)
  flat_key <- key_flatten(key)
  tbl <- as_tibble(.data)
  grped_tbl <- ungroup(tbl) %>% 
    grouped_df(vars = flat_key)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = tbl), int)
    ref_data <- grped_tbl %>% 
      summarise(
        !! idx_chr := list(tibble(!! idx_chr := idx_full))
      ) %>% 
      tidyr::unnest(!! idx)
  } else {
    ref_data <- grped_tbl %>% 
      summarise(
        !! idx_chr := list(tibble(!! idx_chr := seq_generator(!! idx, int)))
      ) %>% 
      unnest(!! idx)
  }
  full_data <- ungroup(ref_data) %>% 
    left_join(.data, by = c(flat_key, idx_chr))

  cn <- names(.data)
  lst_exprs <- exprs(..., .named = TRUE)
  if (!is_empty(lst_exprs)) {
    lhs <- names(lst_exprs)
    check_names <- lhs %in% cn
    if (is_false(all(check_names))) {
      bad_names <- paste_comma(lhs[which(!check_names)])
      abort(sprintf("Can't find column `%s` in `.data`.", bad_names))
    }
    replaced_df <- tbl %>% 
      summarise(!!! lst_exprs) %>% 
      ungroup() %>% 
      select(!!! lhs)
    full_data <- replace_na2(full_data, replaced_df, group_vars(tbl))
  }
  if (!identical(cn, names(full_data))) {
    full_data <- full_data %>%
      select(!!! syms(cn)) # keep the original order
  }
  update_tsibble(full_data, .data, interval = interval(.data))
}

#' Count implicit gaps
#' 
#' @param .data A `tbl_ts`.
#' @param ... Other arguments passed on to individual methods.
#'
#' @rdname count-gaps
#' @export
#' @seealso [fill_na]
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * "from": the starting time point of the gap
#' * "end": the ending time point of the gap
#' * "n": the implicit missing observations during the time period
count_gaps <- function(.data, ...) {
  UseMethod("count_gaps")
}

#' @rdname count-gaps
#' @param .full `FALSE` to find gaps for each group within its own period. `TRUE`
#' to find gaps over the entire time span of the data.
#' @export
#' @examples
#' ped_gaps <- pedestrian %>% 
#'   count_gaps(.full = TRUE)
#' if (!requireNamespace("ggplot2", quietly = TRUE)) {
#'   stop("Please install the ggplot2 package to run these following examples.")
#' }
#' library(ggplot2)
#' ggplot(ped_gaps, aes(colour = Sensor)) +
#'   geom_linerange(aes(x = Sensor, ymin = from, ymax = to)) +
#'   geom_point(aes(x = Sensor, y = from)) +
#'   geom_point(aes(x = Sensor, y = to)) +
#'   coord_flip() +
#'   theme(legend.position = "bottom")
count_gaps.tbl_ts <- function(.data, .full = FALSE, ...) {
  not_regular(.data)
  unknown_interval(int <- interval(.data))

  idx <- index(.data)
  if (!is_grouped_ts(.data)) {
    .data <- group_by(.data, !!! flatten(key(.data)))
  }
  grped_tbl <- as_grouped_df(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = .data), int)
    out <- grped_tbl %>% 
      summarise(gaps = list(gaps(!! idx, idx_full))) %>% 
      unnest(gaps)
  } else {
    out <- grped_tbl %>% 
      summarise(gaps = list(gaps(!! idx, seq_generator(!! idx, int)))) %>% 
      unnest(gaps)
  }
  ungroup(out)
}

#' Does a tsibble have implict gaps in time?
#'
#' Returns a vector of `TRUE`/`FALSE` corresponding to each key.
#'
#' @inheritParams count_gaps
#' @export
#' @rdname has-gaps
#' @examples
#' harvest <- tsibble(
#'   year = c(2010, 2011, 2013, 2011, 2012, 2013),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = id(fruit), index = year
#' )
#' has_gaps(harvest)
#' has_gaps(harvest, .full = TRUE)
has_gaps <- function(.data, ...) {
  UseMethod("has_gaps")
}

#' @rdname has-gaps
#' @export
has_gaps.tbl_ts <- function(.data, .full = FALSE, ...) {
  not_regular(.data)
  unknown_interval(int <- interval(.data))
  idx <- index(.data)
  if (!is_grouped_ts(.data)) {
    .data <- group_by(.data, !!! flatten(key(.data)))
  }
  grped_tbl <- as_grouped_df(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = .data), int)
    res <- grped_tbl %>% 
      summarise(!! "lgl" := (length(idx_full) - length(!! idx)) > 0)
  } else {
    res <- grped_tbl %>% 
      summarise(
        !! "lgl" := (length(seq_generator(!! idx, int)) - length(!! idx)) > 0
      )
  }
  res[["lgl"]]
}

#' Find missing elements in `x` with respect to `y`
#'
#' @param x,y Atomic vectors. The length of `y` must be greater than the length of `x`.
#' @return A tibble of columns `from`, `to` and `n`.
#' @export
#' @examples
#' gaps(x = c(1:3, 5:6, 9:10), y = 1:10)
gaps <- function(x, y) {
  len_x <- length(x)
  len_y <- length(y)
  if (len_y < len_x) {
    msg <- sprintf(
      "`length(x)` (%d) must not be greater than `length(y)` (%d).",
      len_x, len_y
    )
    abort(msg)
  }
  gap_vec <- logical(length = len_y)
  gap_vec[-match(x, y)] <- TRUE
  gap_rle <- rle_lgl(gap_vec)
  lgl_rle <- gap_rle$values
  gap_idx <- gap_rle$lengths
  to <- cumsum(gap_idx)
  from <- c(1, to[-length(to)] + 1)
  nobs <- gap_idx[lgl_rle]
  if (is_empty(nobs)) {
    tibble(from = NA, to = NA, n = 0L)
  } else {
    tibble(
      from = y[from][lgl_rle],
      to = y[to][lgl_rle],
      n = nobs
    )
  }
}

seq_generator <- function(x, interval = NULL) {
  min_x <- min0(x)
  max_x <- max0(x)
  if (is_null(interval)) {
    interval <- pull_interval(x)
  }
  tunit <- time_unit(interval)
  if (tunit == 0) return(x)
  res <- tryCatch(
    seq(min_x, max_x, tunit),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is_null(res)) return(res)
  # no seq.* available
  tryCatch(
    min_x + seq.int(0, as.double(max_x - min_x), tunit),
    error = function(e) {
      e$call <- NULL
      e$message <- sprintf("Neither `+` nor `seq()` are defined for class %s", class(x)[1L])
      stop(e)
    }
  )
}

#' A thin wrapper of `dplyr::case_when()` if there are `NA`s
#'
#' @param formula A two-sided formula. The LHS expects a vector containing `NA`,
#' and the RHS gives the replacement value.
#'
#' @export
#' @seealso [dplyr::case_when]
#' @keywords internal
#' @examples
#' x <- rnorm(10)
#' x[c(3, 7)] <- NA_real_
#' case_na(x ~ 10)
#' case_na(x ~ mean(x, na.rm = TRUE))
case_na <- function(formula) {
  .Deprecated(msg = "This function will be defunct soon.")
  env_f <- f_env(formula)
  lhs <- eval_bare(f_lhs(formula), env = env_f)
  rhs <- eval_bare(f_rhs(formula), env = env_f)
  dplyr::case_when(is.na(lhs) ~ rhs, TRUE ~ lhs)
}

restore_index_class <- function(new, old) {
  old_idx <- as_string(index(old))
  new_idx <- as_string(index(new))
  class(new[[new_idx]]) <- class(old[[old_idx]])
  if (!identical(interval(new), interval(old))) {
    attr(new, "interval") <- pull_interval(new[[new_idx]])
  }
  new
}

replace_na2 <- function(.data, replace = list(), grp_vars = character(0)) {
  replace_vars <- intersect(names(replace), names(.data))
  split_data <- split(.data, group_indices(dplyr::grouped_df(.data, grp_vars)))
  for (i in seq_along(split_data)) {
    for (var in replace_vars) {
      split_data[[i]][[var]][is.na(split_data[[i]][[var]])] <- replace[[var]][i]
    }
  }
  dplyr::bind_rows(split_data)
}
