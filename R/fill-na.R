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
  keyed_tbl <- grped_df_by_key(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = keyed_tbl), int)
    ref_data <- keyed_tbl %>% 
      summarise(!! idx_chr := list(tibble(!! idx_chr := idx_full))) %>% 
      tidyr::unnest(!! idx)
  } else {
    ref_data <- keyed_tbl %>% 
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
    replaced_df <- keyed_tbl %>% 
      summarise(!!! lst_exprs) %>% 
      ungroup() %>% 
      select(!!! lhs)
    full_data <- replace_na2(full_data, replaced_df, group_vars(.data))
  }
  if (!identical(cn, names(full_data))) {
    full_data <- full_data %>%
      select(!!! syms(cn)) # keep the original order
  }
  update_tsibble(full_data, .data, interval = interval(.data))
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
