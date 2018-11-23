#' Deprecated functions
#'
#' @param x A data frame.
#' @param ... A list of unquoted variables, separated by commas, to split a dataset.
#' @rdname deprecated
#' @keywords internal
#' @export
#' @examples
#' pedestrian %>% 
#'   split_by(Sensor)
split_by <- function(x, ...) {
  .Deprecated(msg = "This function will be defunct soon.\nPlease use `split()` instead.")
  UseMethod("split_by")
}

#' @export
split_by.tbl_ts <- function(x, ...) {
  quos <- enquos(...)
  if (is_empty(quos)) return(list(x))

  vars_split <- tidyselect::vars_select(names(x), !!! quos)
  grped_df <- grouped_df(x, vars = vars_split)
  res <- split(x, group_indices(grped_df))
  unname(res)
}

#' @export
split_by.data.frame <- split_by.tbl_ts

#' @export
split_by.tbl_df <- split_by.tbl_ts

#' @description
#' A thin wrapper of `dplyr::case_when()` if there are `NA`s
#'
#' @param formula A two-sided formula. The LHS expects a vector containing `NA`,
#' and the RHS gives the replacement value.
#'
#' @export
#' @rdname deprecated
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

#' @rdname deprecated
#' @export
#' @keywords internal
as.tsibble <- function(x, ...) {
  as_tsibble(x, ...)
}

#' @rdname deprecated
#' @export
#' @keywords internal
#' @include gaps.R
fill_na <- fill_gaps.tbl_ts


#' @description
#' Find which row has duplicated key and index elements
#'
#' @rdname deprecated
#' @param data A `tbl_ts` object.
#' @param key Structural variable(s) that define unique time indices, used with
#' the helper [id]. If a univariate time series (without an explicit key),
#' simply call `id()`.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param fromLast `TRUE` does the duplication check from the last of identical
#' elements.
#'
#' @return A logical vector of the same length as the row number of `data`
#' @export
find_duplicates <- function(data, key = id(), index, fromLast = FALSE) {
  key <- use_id(data, !! enquo(key))
  index <- validate_index(data, enquo(index))

  grouped_df(data, vars = key) %>%
    mutate(!! "zzz" := duplicated.default(!! index, fromLast = fromLast)) %>%
    dplyr::pull(!! "zzz")
}
