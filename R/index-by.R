#' Group and collapse by time index
#'
#' `index_by()` is the counterpart of `group_by()` in temporal context, but it
#' only groups the time index. It adds a new column and then group it. The 
#' following operation is applied to each group of the index, similar to 
#' `group_by()` but dealing with index only. `index_by()` + `summarise()` will
#' update the grouping index variable to be the new index. Use `ungroup()` or 
#' `index_by()` with no arguments to remove the index grouping vars.
#'
#' @param .data A `tbl_ts`.
#' @param ... A single name-value pair of expression: a new index on LHS and the 
#' current index on RHS. Or an existing variable to be used as index.
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
#' * A `index_by()`-ed tsibble is indicated by `@` in the "Groups" when 
#' displaying on the screen.
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
#' # Attempt to aggregate to 4-hour interval, with the effects of DST
#' pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   index_by(Date_Time4 = lubridate::floor_date(Date_Time, "4 hour")) %>%
#'   summarise(Total_Count = sum(Count))
#'
#' # Annual trips by Region and State ----
#' tourism %>% 
#'   index_by(Year = lubridate::year(Quarter)) %>% 
#'   group_by(Region, State) %>% 
#'   summarise(Total = sum(Trips))
index_by <- function(.data, ...) {
  UseMethod("index_by")
}

#' @export
index_by.tbl_ts <- function(.data, ...) {
  exprs <- enexprs(..., .named = TRUE)
  if (is_empty(exprs)) {
    attr(.data, "index2") <- index(.data)
    return(.data)
  }
  if (is_false(has_length(exprs, 1))) {
    abort("`index_by()` only accepts one expression.")
  }
  expr_name <- names(exprs)[1]
  idx <- index(.data)
  idx_chr <- as_string(idx)
  if (identical(idx_chr, expr_name)) {
    abort(sprintf("Column `%s` (index) can't be overwritten.", idx_chr))
  }
  idx2 <- sym(expr_name)
  tbl <- 
    group_by(
      mutate(ungroup(.data), !!! exprs),
      !!! groups(.data), !! idx2, .drop = FALSE
    )
  build_tsibble(
    tbl, key_data = key_data(.data), index = !! idx, index2 = !! idx2,
    regular = is_regular(.data), validate = FALSE,
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

rename_index <- function(.data, .vars) {
  names <- names(.vars)
  idx_chr <- index_var(.data)
  idx <- idx_chr == .vars
  if (sum(idx) == 0) return(.data)

  names(.data)[idx] <- new_idx_chr <- names[idx]
  attr(.data, "index") <- sym(new_idx_chr)
  .data
}

rename_index2 <- function(.data, .vars) {
  names <- names(.vars)
  idx2_chr <- index2_var(.data)
  idx <- idx2_chr == .vars
  if (sum(idx) == 0) return(.data)

  names(.data)[idx] <- new_idx2_chr <- names[idx]
  attr(.data, "index2") <- sym(new_idx2_chr)
  .data
}

mutate_index2 <- function(.data, .vars) {
  chr <- intersect(index2_var(.data), .vars)
  if (is_empty(chr)) {
    attr(.data, "index2") <- index(.data)
  } else {
    attr(.data, "index2") <- sym(chr)
  }
  .data
}
