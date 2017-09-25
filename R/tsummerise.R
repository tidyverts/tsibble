#' @title Aggregate over calendar periods
#'
#' @description It computes summary statistics for a tsibble over calendar
#' periods, usually used in combination of [group_by].
#'
#' @param .data A tsibble (of `tbl_ts` class).
#' @param ... Name-value pairs of summary functions. To aggregate tsibble over
#' a certain calendar period, for example yearly aggregates, use [lubridate::year]
#' on the index variable.
#'
#' @author Earo Wang
#' @rdname tsummarise
#'
#' @return A tsibble class.
#'
#' @examples
#'    # pkgs_ts <- as_tsibble(tidypkgs, index = date, package)
#'    # pkgs_ts %>% 
#'    #   group_by(package) %>% 
#'    # summarise(avg_count = mean(count), month = ~ as.yearmon())
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
  old_class <- class(.data)

  # check if the index variable is present in the function call
  vec_vars <- as.character(purrr::map(lst_quos, ~ lang_args(.)[[1]]))
  idx_var <- format(index)
  idx_pos <- match(idx_var, vec_vars)
  if (is.na(idx_pos)) {
    abort(paste("Missing index variable:", idx_var))
  }
  idx_sym <- sym(names(lst_quos)[[idx_pos]])

  # aggregate over time
  tmp_grps <- c(grps, idx_sym)
  tmp_data <- .data %>% 
    ungroup() %>% 
    mutate(!! idx_sym := !! lst_quos[[idx_pos]]) %>% 
    group_by(!!! tmp_grps)
  tmp_data <- replace_class(tmp_data, "grouped_ts", "grouped_df")
  .data <- tmp_data %>% 
    dplyr::summarise(!!! lst_quos[-idx_pos])

  tbl <- as_tsibble(.data, !!! grps, index = !! idx_sym, validate = FALSE)
  attr(tbl, "vars") <- grps
  class(tbl) <- old_class
  tbl
}

#' @rdname tsummarise
#' @export
tsummarize <- tsummarise
