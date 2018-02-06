#' Aggregate over calendar periods
#'
#' It computes summary statistics for a tsibble over calendar periods, usually 
#' used in combination of [group_by].
#'
#' @param .data A data frame (of `tbl_ts` class).
#' @param ... Name-value pairs of expressions. The index variable must be present
#' in the first name-value pair, with an index function. The remaining components
#' work like `summarise()`.
#' The index functions that can be used, but not limited:
#' * [lubridate::year]: yearly aggregation
#' * [yearquarter]: quarterly aggregation
#' * [yearmonth]: monthly aggregation
#' * [as.Date] or [lubridate::as_date]: daily aggregation
#' * [lubridate::ceiling_date] or [lubridate::round_date]: sub-daily aggregation
#' * other index functions from other packages
#'
#' @details
#' The rightmost grouping level will be dropped.
#'
#' @rdname tsummarise
#' @export
#' @examples
#' # Monthly counts across Sensors
#' data(pedestrian)
#' monthly_ped <- pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   tsummarise(
#'     Year_Month = yearmonth(Date_Time), # Year_Month will be the new index
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#' monthly_ped
#' index(monthly_ped)
#'
#' # Annual trips by Region and State ----
#' data(tourism)
#' tourism %>% 
#'   group_by(Region | State) %>% 
#'   tsummarise(Year = lubridate::year(Quarter), Total = sum(Trips))
tsummarise <- function(.data, ...) {
  UseMethod("tsummarise")
}

#' @export
tsummarise.tbl_ts <- function(.data, ...) {
  lst_quos <- quos(..., .named = TRUE)
  index <- index(.data)
  grps <- groups(.data)

  # check if the index variable is present in the first call
  first_quo <- lst_quos[1]
  first_var <- as.character(first_arg(first_quo))
  idx_var <- quo_text2(index)
  if (is_false(has_index_var(j = first_var, x = .data))) {
    abort(sprintf("Can't find `index` (`%s`) in the first name-value pair.", idx_var))
  }
  idx_name <- names(first_quo)
  idx_sym <- sym(idx_name)

  # aggregate over time
  chr_grps <- c(flatten_key(grps), idx_name) 
  pre_data <- .data %>% 
    ungroup() %>% 
    mutate(!!! first_quo, drop = TRUE)
  result <- pre_data %>% 
    grouped_df(vars = chr_grps) %>% 
    summarise(!!! lst_quos[-1])

  as_tsibble(
    result, key = grps, index = !! idx_sym, groups = drop_group(grps), 
    validate = FALSE
  )
}

#' @rdname tsummarise
#' @export
tsummarize <- tsummarise
