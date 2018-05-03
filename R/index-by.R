#' Prepare to aggregate over time and update index
#'
#' In spirit of `group_by()`, `index_by()` prepares for a new index with a less 
#' granular period of time. `summarise()` will be operated on each group of the
#' index, in conjunction with `index_by()`.
#'
#' @param .data A `tbl_ts`.
#' @param ... A name-value pair of expression or a bare variable.
#' The index functions that can be used, but not limited:
#' * [lubridate::year]: yearly aggregation
#' * [yearquarter]: quarterly aggregation
#' * [yearmonth]: monthly aggregation
#' * [yearweek]: weekly aggregation
#' * [as.Date] or [lubridate::as_date]: daily aggregation
#' * [lubridate::ceiling_date], [lubridate::floor_date], or [lubridate::round_date]: 
#' sub-daily aggregation
#' * other index functions from other packages
#'
#' @details
#' * A `index_by()`-ed tsibble is indicated by `*` in the printing output.
#' * `index_by()` only works with `summarise()`, not other verbs.
#' * Time index will not be collapsed by `summarise.tbl_ts`.
#' * The scoped variants of `summarise()` only operate on the non-key and 
#' non-index variables.
#'
#' @rdname index-by
#' @export
#' @examples
#' # Monthly counts across sensors ----
#' monthly_ped <- pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   index_by(Year_Month = yearmonth(Date_Time)) %>%
#'   summarise(
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#' monthly_ped
#' index(monthly_ped)
#' 
#' # Using existing variable ----
#' pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   index_by(Date) %>%
#'   summarise(
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#'
#' # Annual trips by Region and State ----
#' tourism %>% 
#'   index_by(Year = lubridate::year(Quarter)) %>% 
#'   group_by(Region | State) %>% 
#'   summarise(Total = sum(Trips))
index_by <- function(.data, ...) {
  exprs <- enexprs(..., .named = TRUE)
  if (is_false(has_length(exprs, 1))) {
    abort("`index_by()` only accepts one expression")
  }
  expr <- exprs[[1]]
  if (is_call(expr)) {
    idx <- first_arg(exprs)[[1]]
  } else if (is_symbol(expr)) {
    idx <- expr
  } else {
    abort(sprintf(
      "`index_by()` accepts either a call or a name, not %s.",
      class(expr)[[1]]
    ))
  }
  validate_vars(idx, names(.data))

  attr(.data, "idx") <- exprs
  .data
}
