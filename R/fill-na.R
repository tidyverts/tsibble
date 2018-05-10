globalVariables(".")

#' Turn implicit missing values into explicit missing values
#'
#' @param .data A data frame.
#' @param ... A set of name-value pairs. The values will replace existing explicit
#' missing values by variable, otherwise `NA`. The replacement values must be of
#' the same type as the original one. If using a function to fill the `NA`,
#' please make sure that `na.rm = TRUE` is switched on.
#'
#' @seealso [case_na], [tidyr::fill], [tidyr::replace_na]
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
#'   fill(kilo, .direction = "down")
#'
#' # replace NA with a specific value ----
#' harvest %>%
#'   fill_na(kilo = 0L)
#'
#' # replace NA using a function by variable ----
#' # enable `na.rm = TRUE` when necessary ----
#' harvest %>%
#'   fill_na(kilo = sum(kilo, na.rm = TRUE))
#'
#' # replace NA using a function for each group ----
#' harvest %>%
#'   group_by(fruit) %>%
#'   fill_na(kilo = sum(kilo, na.rm = TRUE))
#'
#' # replace NA ----
#' pedestrian %>%
#'   group_by(Sensor) %>%
#'   fill_na(
#'     Date = lubridate::as_date(Date_Time),
#'     Time = lubridate::hour(Date_Time),
#'     Count = as.integer(median(Count, na.rm = TRUE))
#'   )
#' @export
fill_na.tbl_ts <- function(.data, ..., .full = FALSE) {
  not_regular(.data)
  idx <- index(.data)
  idx_chr <- quo_text(idx)
  key <- key(.data)
  flat_key <- key_flatten(key)
  tbl <- ungroup(as_tibble(.data))
  grped_tbl <- tbl %>% 
    grouped_df(vars = flat_key)
  if (.full) {
    idx_full <- seq_by(eval_tidy(idx, data = tbl))
    ref_data <- grped_tbl %>% 
      summarise(
        !! idx_chr := list(tibble::tibble(!! idx_chr := idx_full))
      ) %>% 
      tidyr::unnest(!! idx)
  } else {
    ref_data <- grped_tbl %>% 
      summarise(
        !! idx_chr := list(tibble::tibble(!! idx_chr := seq_by(!! idx)))
      ) %>% 
      tidyr::unnest(!! idx)
  }
  full_data <- ungroup(ref_data) %>% 
    left_join(.data, by = c(flat_key, idx_chr))

  if (is_grouped_ts(.data)) {
    full_data <- full_data %>% 
      grouped_df(vars = group_vars(.data)) %>% 
      do(modify_na(., !!! enquos(...)))
  } else {
    full_data <- full_data %>%
      modify_na(!!! enquos(...))
  }
  full_data <- full_data %>%
    select(!!! syms(names(.data))) # keep the original order
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
#' @seealso fill_na
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * "from": the starting time point of the gap
#' * "end": the ending time point of the gap
#' * "n": the implicit missing observations during the time period
count_gaps <- function(.data, ...) {
  not_regular(.data)
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
  idx_full <- seq_by(eval_tidy(idx, data = .data))
  ungroup(as_tibble(.data)) %>% 
    summarise(gaps = list(gaps(unique.default(!! idx), idx_full))) %>% 
    tidyr::unnest(gaps)
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
    idx_full <- seq_by(eval_tidy(idx, data = tbl))
    out <- tbl %>% 
      summarise(gaps = list(gaps(!! idx, idx_full))) %>% 
      tidyr::unnest(gaps)
  } else {
    out <- tbl %>% 
      summarise(gaps = list(gaps(!! idx, seq_by(!! idx)))) %>% 
      tidyr::unnest(gaps)
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
    return(tibble::tibble(from = NA, to = NA, n = 0L))
  }
  tibble::tibble(
    from = y[from][lgl_rle],
    to = y[to][lgl_rle],
    n = nobs
  )
}

seq_by <- function(x) {
  seq(from = min0(x), to = max0(x), by = time_unit(x))
}

modify_na <- function(.data, ...) {
  lst_quos <- enquos(..., .named = TRUE)
  if (is_empty(lst_quos)) {
    return(.data)
  }
  lhs <- names(lst_quos)
  check_names <- lhs %in% colnames(.data)
  if (is_false(all(check_names))) {
    bad_names <- paste_comma(lhs[which(!check_names)])
    abort(sprintf("Can't find column `%s` in `.data`.", bad_names))
  }

  rhs <- purrr::map(lst_quos, f_rhs)
  lst_lang <- purrr::map2(
    syms(lhs), rhs, ~ new_formula(.x, .y, env = env(!!! .data))
  )
  mod_quos <- purrr::map(lst_lang, ~ call2("case_na", .))
  names(mod_quos) <- lhs
  modify_na_handler(.data, mod_quos)
}

modify_na_handler <- function(.data, quos) {
  tryCatch(
    mutate(.data, !!! quos),
    error = function(e) {
      e$call <- "fill_na(.data, ...)"
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

restore_index_class <- function(data, newdata) {
  old_idx <- quo_text(index(data))
  new_idx <- quo_text(index(newdata))
  class(newdata[[new_idx]]) <- class(data[[old_idx]])
  newdata
}

not_regular <- function(x) {
  if (!is_regular(x)) {
    abort("Can't handle `tbl_ts` of irregular interval.")
  }
}
