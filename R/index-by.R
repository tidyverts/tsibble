#' Group by time index and collapse with `summarise()`
#'
#' @description
#' \lifecycle{stable}
#'
#' `index_by()` is the counterpart of `group_by()` in temporal context, but it
#' only groups the time index. The following operation is applied to each partition
#' of the index, similar to `group_by()` but dealing with index only.
#' `index_by()` + `summarise()` will update the grouping index variable to be
#' the new index. Use `ungroup()` to remove the index grouping vars.
#'
#' @param .data A `tbl_ts`.
#' @param ... If empty, grouping the current index. If not empty, a single
#' expression is required for either an existing variable or a name-value pair.
#' A lambda expression is supported, for example `~ as.Date(.)` where `.` refers
#' to the index variable.
#' The index functions that can be used, but not limited:
#' * [lubridate::year]: yearly aggregation
#' * [yearquarter]: quarterly aggregation
#' * [yearmonth]: monthly aggregation
#' * [yearweek]: weekly aggregation
#' * [as.Date] or [lubridate::as_date]: daily aggregation
#' * [lubridate::ceiling_date], [lubridate::floor_date], or [lubridate::round_date]:
#' fine-resolution aggregation
#' * Extract time components functions, such as [lubridate::hour()] & [lubridate::day()]
#' * other index functions from other packages or self-defined functions
#'
#' @details
#' * A `index_by()`-ed tsibble is indicated by `@` in the "Groups" when
#' displaying on the screen.
#'
#' @rdname index-by
#' @export
#' @examples
#' pedestrian %>% index_by()
#' # Monthly counts across sensors
#' library(dplyr, warn.conflicts = FALSE)
#' monthly_ped <- pedestrian %>%
#'   group_by_key() %>%
#'   index_by(Year_Month = ~ yearmonth(.)) %>%
#'   summarise(
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#' monthly_ped
#' index(monthly_ped)
#'
#' # Using existing variable
#' pedestrian %>%
#'   group_by_key() %>%
#'   index_by(Date) %>%
#'   summarise(
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#'
#' # Attempt to aggregate to 4-hour interval, with the effects of DST
#' pedestrian %>%
#'   group_by_key() %>%
#'   index_by(Date_Time4 = ~ lubridate::floor_date(., "4 hour")) %>%
#'   summarise(Total_Count = sum(Count))
#'
#' library(lubridate, warn.conflicts = FALSE)
#' # Annual trips by Region and State
#' tourism %>%
#'   index_by(Year = ~ year(.)) %>%
#'   group_by(Region, State) %>%
#'   summarise(Total = sum(Trips))
#'
#' # Rouding to financial year, using a custom function
#' financial_year <- function(date) {
#'   year <- year(date)
#'   ifelse(quarter(date) <= 2, year, year + 1)
#' }
#' tourism %>%
#'   index_by(Year = ~ financial_year(.)) %>%
#'   summarise(Total = sum(Trips))
index_by <- function(.data, ...) {
  UseMethod("index_by")
}

#' @export
index_by.tbl_ts <- function(.data, ...) {
  exprs <- enquos(...)
  if (length(exprs) > 1) {
    abort("`index_by()` only accepts one expression or empty.")
  }

  idx <- index_var(.data)
  if (identical(idx, names(exprs))) {
    abort(sprintf("Column `%s` (index) can't be overwritten.", idx))
  }
  idx2_data <- ungrp <- new_data_frame(.data)
  if (is_empty(exprs)) {
    idx2 <- index_var(.data)
  } else {
    idx2 <- names(quos_auto_name(exprs))
    expr <- exprs[[1]]
    expr_f <- quo_get_expr(expr)
    if (is_formula(expr_f)) { # lambda expression
      f <- as_function(eval_bare(expr_f), env = quo_get_env(expr))
      idx2_data <- mutate(ungrp, !!idx2 := f(!!sym(idx)))
    } else {
      idx2_data <- mutate(ungrp, !!idx2 := !!expr)
    }
  }
  tbl <- grouped_df(idx2_data, union(group_vars(.data), idx2), drop = FALSE)
  build_tsibble(tbl,
    key_data = key_data(.data), index = !!idx, index2 = !!idx2,
    ordered = is_ordered(.data), interval = interval(.data),
    validate = FALSE
  )
}

mutate_index2 <- function(.data, .vars) {
  chr <- intersect(index2_var(.data), .vars)
  if (!is_empty(chr)) {
    attr(.data, "index2") <- chr
  } else {
    attr(.data, "index2") <- index_var(.data)
  }
  .data
}
