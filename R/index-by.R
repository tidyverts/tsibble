#' Prepare to aggregate over time and update index
#'
#' In spirit of `group_by()`, `index_by()` prepares for a new index with a less 
#' granular period of time. `summarise()` will be operated on each group of the
#' index, in conjunction with `index_by()`.
#'
#' @param .data A `tbl_ts`.
#' @param ... 
#' * A single name-value pair of expression: a new index on LHS and the current 
#' index on RHS 
#' * An existing variable to be used as index
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
      "`index_by()` accepts either a call or a name, not %s.", class(expr)[[1]]
    ))
  }
  cn <- names(.data)
  val_vars <- validate_vars(idx, cn)
  is_index_in_keys <- intersect(val_vars, key_vars(.data))
  if (is_false(is_empty(is_index_in_keys))) {
    abort(sprintf("`%s` can't be `index`, as it's used as `key`.", val_vars))
  }
  idx_type <- purrr::map_chr(.data, index_sum)
  idx_na <- idx_type[val_vars]
  if (is.na(idx_na)) {
    cls_idx <- purrr::map_chr(.data, ~ class(.)[1])
    name_idx <- names(idx_na)
    abort(sprintf(
      "Unsupported index type: `%s`", cls_idx[cn %in% name_idx])
    )
  }

  attr(.data, "idx") <- exprs
  .data
}
