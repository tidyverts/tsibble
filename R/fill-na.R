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
  if (!is_regular(.data)) {
    abort("Can't handle `tbl_ts` of irregular interval.")
  }
  idx <- index(.data)
  idx_chr <- quo_text2(idx)
  key <- key(.data)
  flat_key <- flatten(key)
  if (.full) {
    full_data <- .data %>%
      tidyr::complete(
        !! idx_chr := seq_by(!! idx),
        tidyr::nesting(!!! flat_key)
      )
  } else {
    full_data <- as_tibble(.data) %>% 
      split_by(!!! flat_key) %>% 
      purrr::map_dfr(~ tidyr::complete(.,
        !! idx_chr := seq_by(!! idx),
        tidyr::nesting(!!! flat_key)
      ))
  }

  if (is_grouped_ts(.data)) {
    full_data <- do(full_data, modify_na(., !!! enquos(...)))
  } else {
    full_data <- full_data %>%
      modify_na(!!! enquos(...))
  }
  full_data <- full_data %>%
    select(!!! syms(colnames(.data))) # keep the original order
  # ensure the time ordering
  tsbl <- build_tsibble(
    full_data, key = key, index = !! idx, 
    validate = FALSE, ordered = NULL
  )
  restore_index_class(.data, tsbl)
}

#' Count implicit gaps
#'
#' `count_gaps()` counts gaps for a tsibble; `gaps()` find where the gaps in `x` 
#' with respect to `y`.
#' 
#' @param .data A `tbl_ts`.
#' @inheritParams fill_na
#'
#' @rdname gaps
#' @export
#' @seealso fill_na
#' @examples
#' count_gaps(pedestrian)
#' count_gaps(pedestrian, .full = TRUE)
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * "from": the starting time point of the gap
#' * "end": the ending time point of the gap
#' * "n": the implicit missing observations during the time period
count_gaps <- function(.data, .full = FALSE) {
  if (!is_regular(.data)) {
    abort("Can't handle `tbl_ts` of irregular interval.")
  }
  idx <- index(.data)
  flat_key <- flatten(key(.data))
  tbl <- as_tibble(.data)

  grped_tbl <- tbl %>% 
    group_by(!!! flat_key)
  if (.full) {
    idx_full <- seq_by(eval_tidy(idx, data = tbl))
    out <- grped_tbl %>% 
      summarise(gaps = list(gaps(!! idx, idx_full))) %>% 
      tidyr::unnest(gaps)
  } else {
    out <- grped_tbl %>% 
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
  gap_rle <- rle(gap_vec)
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
  old_idx <- quo_text2(index(data))
  new_idx <- quo_text2(index(newdata))
  class(newdata[[new_idx]]) <- class(data[[old_idx]])
  newdata
}
