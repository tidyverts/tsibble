#' Aggregate over calendar periods
#'
#' It computes summary statistics for a tsibble over calendar periods, usually 
#' used in combination of `group_by`.
#'
#' @param .data A data frame (of `tbl_ts` class).
#' @param ... Name-value pairs of expressions. The index variable must be present
#' in the first name-value pair, with an index function. The remaining components
#' work like `summarise()`. For the scoped variants like `_all()`, `_at()`, `_if()`,
#' additional arguments for the function call in `.funs` will be ignored in `...`.
#' The index functions that can be used, but not limited:
#' * [lubridate::year]: yearly aggregation
#' * [yearquarter]: quarterly aggregation
#' * [yearmonth]: monthly aggregation
#' * [yearweek]: weekly aggregation
#' * [as.Date] or [lubridate::as_date]: daily aggregation
#' * [lubridate::ceiling_date], [lubridate::floor_date], or [lubridate::round_date]: 
#' sub-daily aggregation
#' * other index functions from other packages
#' @inheritParams dplyr::summarise_all
#'
#' @details
#' * For a grouped tsibble, the rightmost grouping variable will be dropped 
#' after the operation.
#' * The scoped variants only operate on the non-key and non-index variables.
#'
#' @seealso [dplyr::summarise_all]
#' @rdname tsibble-deprecated
#' @export
tsummarise <- function(.data, ...) {
  .Deprecated("index_by() + summarise()", package = "tsibble")
  UseMethod("tsummarise")
}

#' @export
tsummarise.tbl_ts <- function(.data, ...) {
  lst_quos <- separate_quos(warn = FALSE, ...)
  tsum(.data, lst_quos$first, lst_quos$remainder, FUN = summarise)
}

#' @rdname tsibble-deprecated
#' @export
tsummarise_all <- function(.data, ..., .funs) {
  lst_quos <- separate_quos(warn = TRUE, ...)
  tsum(
    .data, lst_quos$first, remainder = NULL, 
    FUN = function(x) dplyr::summarise_all(x, .funs = .funs)
  )
}

#' @rdname tsibble-deprecated
#' @export
tsummarise_if <- function(.data, ..., .predicate, .funs) {
  lst_quos <- separate_quos(warn = TRUE, ...)
  tsum(
    .data, lst_quos$first, remainder = NULL, 
    FUN = function(x) 
      dplyr::summarise_if(x, .predicate = .predicate, .funs = .funs)
  )
}

#' @rdname tsibble-deprecated
#' @export
tsummarise_at <- function(.data, ..., .vars, .funs) {
  lst_quos <- separate_quos(warn = TRUE, ...)
  tsum(
    .data, lst_quos$first, remainder = NULL, 
    FUN = function(x) 
      dplyr::summarise_at(x, .vars = .vars, .funs = .funs)
  )
}

#' @rdname tsibble-deprecated
#' @export
tsummarize <- tsummarise

#' @rdname tsibble-deprecated
#' @export
tsummarize_all <- tsummarise_all

#' @rdname tsibble-deprecated
#' @export
tsummarize_if <- tsummarise_if

#' @rdname tsibble-deprecated
#' @export
tsummarize_at <- tsummarise_at

tsum <- function(.data, first, remainder = NULL, FUN = summarise) {
  index <- index(.data)
  grps <- groups(.data)

  # check if the index variable is present in the first call
  first_var <- as.character(first_arg(first))
  idx_var <- quo_text(index)
  if (is_false(has_index_var(j = first_var, x = .data))) {
    abort(sprintf("Can't find `index` (`%s`) in the first name-value pair.", idx_var))
  }
  idx <- quo_text(index)
  idx_name <- names(first)
  if (idx_name == "") {
    names(first) <- idx_name <- idx
  }
  idx_sym <- sym(idx_name)

  # aggregate over time
  chr_grps <- c(flat_grps <- key_flatten(grps), idx_name) 
  pre_data <- .data %>% 
    ungroup() %>% 
    mutate(!!! first, .drop = TRUE)
  if (idx_name == idx) {
    grped_data <- pre_data %>% 
      grouped_df(vars = chr_grps)
  } else {
    grped_data <- pre_data %>% 
      select(- !! index) %>% 
      grouped_df(vars = chr_grps)
  }
  if (is.null(remainder)) { # summarise_*()
    nonkey <- setdiff(key_flatten(key(.data)), flat_grps)
    sel_cols <- setdiff(names(grped_data), nonkey)
    grped_data <- select(grped_data, !! sel_cols)
    result <- FUN(grped_data)
  } else { # summarise()
    result <- FUN(grped_data, !!! remainder)
  }

  build_tsibble(
    result, key = grps, index = !! idx_sym, groups = drop_group(grps), 
    validate = FALSE, ordered = TRUE
  )
}

separate_quos <- function(warn = FALSE, ...) {
  lst_quos <- enquos(...)
  first <- lst_quos[1]
  remainder <- lst_quos[-1]
  if (warn) {
    if (has_length(remainder)) {
      args <- paste_comma(surround(names(remainder), "`"))
      warn(sprintf("The arguments are ignored: %s", args))
    }
  }
  list(first = first, remainder = remainder)
}
