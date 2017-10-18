#' Turn implicit missing values into explicit missing values
#'
#' @param .data A `tbl_ts`.
#' @param ... A set of name-value pairs. The values will replace existing explicit
#' missing values by variable, otherwise `NA`.
#'
#' @rdname fill-na
#' @export
#'
#' @examples
#' tsbl <- as_tsibble(tibble::tibble(
#'   year = year(c(2010, 2011, 2013, 2011, 2012, 2014)),
#'   group = rep(letters[1:2], each = 3),
#'   value = sample(1:10, size = 6),
#' ), group, index = year)
#'
#' # leave NA as is
#' fill_na(tsbl)
#'
#' # replace NA with a specific number
#' tsbl %>%
#'   fill_na(value = 0L)
#'
#' # replace NA using some functions
#' tsbl %>% 
#'   fill_na(value = sum(value, na.rm = TRUE))
#' 
#' # replace NA
#' pedestrian %>% 
#'   fill_na(
#'     Date = lubridate::as_date(Date_Time),
#'     Time = lubridate::hour(Date_Time),
#'     Count = median(Count, na.rm = TRUE)
#'   )
fill_na <- function(.data, ...) {
  UseMethod("fill_na")
}

#' @rdname fill-na
#' @export
fill_na.tbl_ts <- function(.data, ...) {
  if (!is_regular(.data)) {
    abort("Don't know how to handle irregular time series data.")
  }
  idx <- index(.data)
  full_data <- .data %>%
    tidyr::complete(
      !! quo_text2(idx) := full_seq(!! idx, period = as_period(!! idx)),
      !!! key(.data)
    )

  full_data <- full_data %>%
    mutate_na(!!! quos(...))
  full_data <- full_data[, colnames(.data)] # keep the original order
  as_tsibble(full_data, !!! key(.data), index = !! idx, validate = FALSE)
}

# ToDo: I'd like to use group_by() in conjunction
# I think that the issue arises when creating new formula which is not evaluated
# in the grouped df
mutate_na <- function(.data, ...) {
  lst_quos <- quos(..., .named = TRUE)
  if (is_empty(lst_quos)) {
    return(.data)
  }
  lhs <- names(lst_quos)
  check_names <- lhs %in% colnames(.data)
  if (is_false(all(check_names))) {
    bad_names <- paste(lhs[-which(check_names)], collapse = ", ")
    abort(paste("Unexpected LHS names:", bad_names))
  }

  rhs <- purrr::map(lst_quos, f_rhs)
  lst_lang <- purrr::map2(
    syms(lhs), rhs, ~ new_formula(.x, .y, env = env(!!! .data))
  )
  mod_quos <- purrr::map(lst_lang, ~ lang("case_na", .))
  names(mod_quos) <- lhs
  .data %>%
    mutate(!!! mod_quos)
}

case_na <- function(formula) {
  env_f <- f_env(formula)
  lhs <- eval_bare(f_lhs(formula), env = env_f)
  rhs <- eval_bare(f_rhs(formula), env = env_f)
  dplyr::case_when(is.na(lhs) ~ rhs, TRUE ~ lhs)
}

# complete.tbl_ts <- function(data, ..., fill = list()) {
#   grps <- groups(data)
#   comp_data <- NextMethod()
#   if (is_grouped_ts(data)) {
#     comp_data <- dplyr::grouped_df(comp_data, vars = flatten_key(grps))
#     groups(comp_data) <- grps
#   }
#   as_tsibble(
#     comp_data, !!! key(data), index = !! index(data),
#     validate = FALSE, regular = is_regular(data)
#   )
# }

#' @export
full_seq.integer <- function(x, period, tol = 1e-06) {
  x <- as.numeric(x)
  year(full_seq(x, period = period, tol = tol))
}

#' @export
full_seq.yearmth <- function(x, period, tol = 1e-06) {
  rng <- range(x, na.rm = TRUE)
  if (any((x - rng[1]) %% period > tol)) {
      abort("`x` is not a regular sequence.")
  }
  seq(rng[1], rng[2], by = period)
}

#' @export
full_seq.yearqtr <- full_seq.yearmth
