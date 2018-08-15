globalVariables(".")

#' Turn implicit missing values into explicit missing values
#'
#' @param .data A data frame.
#' @param ... A set of name-value pairs. The values will replace existing explicit
#' missing values by variable, otherwise `NA`. The replacement values must be of
#' the same type as the original one.
#'
#' @seealso [count_gaps], [case_na], [tidyr::fill], [tidyr::replace_na]
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
#' # enable `na.rm = TRUE` when necessary ----
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
  unknown_interval(interval(.data))
  idx <- index(.data)
  idx_chr <- quo_text(idx)
  key <- key(.data)
  flat_key <- key_flatten(key)
  tbl <- as_tibble(.data)
  grped_tbl <- ungroup(tbl) %>% 
    grouped_df(vars = flat_key)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = tbl))
    ref_data <- grped_tbl %>% 
      summarise(
        !! idx_chr := list(tibble::tibble(!! idx_chr := idx_full))
      ) %>% 
      tidyr::unnest(!! idx)
  } else {
    ref_data <- grped_tbl %>% 
      summarise(
        !! idx_chr := list(tibble::tibble(!! idx_chr := seq_generator(!! idx)))
      ) %>% 
      unnest(!! idx)
  }
  full_data <- ungroup(ref_data) %>% 
    left_join(.data, by = c(flat_key, idx_chr))

  cn <- names(.data)
  lst_quos <- enquos(..., .named = TRUE)
  if (!is_empty(lst_quos)) {
    lhs <- names(lst_quos)
    check_names <- lhs %in% cn
    if (is_false(all(check_names))) {
      bad_names <- paste_comma(lhs[which(!check_names)])
      abort(sprintf("Can't find column `%s` in `.data`.", bad_names))
    }
    replaced_df <- tbl %>% 
      summarise(!!! lst_quos) %>% 
      ungroup() %>% 
      select(!!! lhs)
    full_data <- replace_na2(full_data, replaced_df)
  }
  if (!identical(cn, names(full_data))) {
    full_data <- full_data %>%
      select(!!! syms(cn)) # keep the original order
  }
  update_tsibble(full_data, .data, interval = interval(.data))
}

#' Count implicit gaps
#'
#' `count_gaps()` counts gaps for a tsibble; `gaps()` find where the gaps in `x` 
#' with respect to `y`.
#' 
#' @param .data A `tbl_ts`.
#' @param ... Other arguments passed on to individual methods.
#'
#' @rdname gaps
#' @export
#' @seealso [fill_na]
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * "from": the starting time point of the gap
#' * "end": the ending time point of the gap
#' * "n": the implicit missing observations during the time period
count_gaps <- function(.data, ...) {
  not_regular(.data)
  unknown_interval(interval(.data))
  UseMethod("count_gaps")
}

#' @rdname gaps
#' @export
#' @examples
#' # Implicit missing time without group_by ----
#' # All the sensors have 2 common missing time points in the data
#' count_gaps(pedestrian)
count_gaps.tbl_ts <- function(.data, ...) {
  idx <- index(.data)
  idx_full <- seq_generator(eval_tidy(idx, data = .data))
  ungroup(as_tibble(.data)) %>% 
    summarise(gaps = list(gaps(unique.default(!! idx), idx_full))) %>% 
    unnest(gaps)
}

#' @rdname gaps
#' @param .full `FALSE` to find gaps for each group within its own period. `TRUE`
#' to find gaps over the entire time span of the data.
#' @export
#' @examples
#' # Time gaps for each sensor per month ----
#' pedestrian %>% 
#'   index_by(yrmth = yearmonth(Date)) %>% 
#'   group_by(Sensor) %>% 
#'   count_gaps()
#' # Time gaps for each sensor ----
#' ped_gaps <- pedestrian %>% 
#'   group_by(Sensor) %>% 
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
count_gaps.grouped_ts <- function(.data, .full = FALSE, ...) {
  idx <- index(.data)
  tbl <- as_tibble(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = tbl))
    out <- tbl %>% 
      summarise(gaps = list(gaps(!! idx, idx_full))) %>% 
      unnest(gaps)
  } else {
    out <- tbl %>% 
      summarise(gaps = list(gaps(!! idx, seq_generator(!! idx)))) %>% 
      unnest(gaps)
  }
  ungroup(out)
}

#' @rdname gaps
#' @param x,y A vector of numbers, dates, or date-times. The length of `y` must
#' be greater than the length of `x`.
#' @export
#' @examples
#' # Vectors ----
#' gaps(x = c(1:3, 5:6, 9:10), y = 1:10)
gaps <- function(x, y) {
  len_x <- length(x)
  len_y <- length(y)
  if (len_y < len_x) {
    msg <- sprintf(
      "The length of `x` (%d) must not be greater than the length of `y` (%d).",
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
    tibble::tibble(from = NA, to = NA, n = 0L)
  } else {
    tibble::tibble(
      from = y[from][lgl_rle],
      to = y[to][lgl_rle],
      n = nobs
    )
  }
}

# has_gaps <- function(.data, .full = FALSE) {
#   grouped_df(.data, key_vars(.data)) %>% 
#     mutate(!! "diff" := difference(as.double(!! index(.data)))) %>% 
#     summarise(!! "lgl" := any_not_equal_to_c(diff[-1], diff[2])) %>% 
#     dplyr::pull("lgl") %>% 
#     any()
# }

seq_generator <- function(x) {
  min_x <- min0(x)
  max_x <- max0(x)
  tunit <- time_unit(x)
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
      e$message <- sprintf("Neither `+` nor `seq()` are defined for class `%s`", class(x))
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
#' @examples
#' x <- rnorm(10)
#' x[c(3, 7)] <- NA_real_
#' case_na(x ~ 10)
#' case_na(x ~ mean(x, na.rm = TRUE))
case_na <- function(formula) {
  env_f <- f_env(formula)
  lhs <- eval_bare(f_lhs(formula), env = env_f)
  rhs <- eval_bare(f_rhs(formula), env = env_f)
  dplyr::case_when(is.na(lhs) ~ rhs, TRUE ~ lhs)
}

restore_index_class <- function(new, old) {
  old_idx <- quo_text(index(old))
  new_idx <- quo_text(index(new))
  class(new[[new_idx]]) <- class(old[[old_idx]])
  if (!identical(interval(new), interval(old))) {
    attr(new, "interval") <- pull_interval(new[[new_idx]])
  }
  new
}

not_regular <- function(x) {
  if (!is_regular(x)) {
    abort("Can't handle `tbl_ts` of irregular interval.")
  }
}

replace_na2 <- function(.data, replace = list()) {
  replace_vars <- intersect(names(replace), names(.data))
  for (var in replace_vars) {
    .data[[var]][is.na(.data[[var]])] <- replace[[var]]
  }
  .data
}
