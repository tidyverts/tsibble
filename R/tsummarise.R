#' Aggregate over calendar periods
#'
#' It computes summary statistics for a tsibble over calendar periods, usually 
#' used in combination of [group_by].
#'
#' @param .data A tsibble (of `tbl_ts` class).
#' @param ... Name-value pairs of summary functions. To aggregate the data over
#' a certain calendar period, for example monthly aggregates, use [yearmth] 
#' on the index variable.
#'
#' @rdname tsummarise
#'
#' @examples
#' monthly_ped <- pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   tsummarise(
#'     Year_Month = yearmth(Date_Time), # Year_Month will be the new index
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#' monthly_ped
#' index(monthly_ped)
#'
#'
#' @export
tsummarise <- function(.data, ...) {
  UseMethod("tsummarise")
}

#' @export
tsummarise.tbl_ts <- function(.data, ...) {
  lst_quos <- quos(..., .named = TRUE)
  index <- index(.data)
  grps <- groups(.data)

  # check if the index variable is present in the function call
  vec_vars <- as.character(purrr::map(lst_quos, ~ lang_args(.)[[1]]))
  idx_var <- quo_text2(index)
  if (is_false(has_index_var(j = vec_vars, x = .data))) {
    abort(paste("Missing index variable:", idx_var))
  }
  idx_pos <- match(idx_var, vec_vars)
  idx_name <- names(lst_quos)[[idx_pos]]
  idx_sym <- sym(idx_name)

  # aggregate over time
  chr_grps <- c(flatten_key(grps), idx_name) 
  pre_data <- .data %>% 
    ungroup() %>% 
    mutate(!! idx_sym := !! lst_quos[[idx_pos]], drop = TRUE)
  result <- pre_data %>% 
    dplyr::grouped_df(vars = chr_grps) %>% 
    dplyr::summarise(!!! lst_quos[-idx_pos])

  tbl <- as_tsibble(result, !!! grps, index = !! idx_sym, validate = FALSE)
  groups(tbl) <- grps
  tbl
}

#' @rdname tsummarise
#' @export
tsummarize <- tsummarise
