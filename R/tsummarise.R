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
  lst_quos <- separate_quos(warn = FALSE, ...)
  tsum(.data, lst_quos$first, lst_quos$remainder, FUN = summarise)
}

#' @rdname tsummarise
#' @export
#' @examples
#' # scoped variants ----
#' tsbl <- tsibble(
#'   qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   a = rnorm(30),
#'   b = rnorm(30),
#'   c = rnorm(30),
#'   key = id(group), index = qtr
#' )
#' tsbl %>% 
#'   group_by(group) %>% 
#'   tsummarise_all(year = lubridate::year(qtr), .funs = mean)
#' tsbl %>% 
#'   group_by(group) %>% 
#'   tsummarise_if(
#'      year = lubridate::year(qtr), 
#'      .predicate = is.numeric, .funs = sum
#'   )
#' # additional arguments need putting into the `.funs`
#' tsbl %>% 
#'   group_by(group) %>% 
#'   tsummarise_at(
#'      year = lubridate::year(qtr), 
#'      .vars = c("a", "c"), .funs = function(x) median(x, na.rm = TRUE)
#'   )
tsummarise_all <- function(.data, ..., .funs) {
  lst_quos <- separate_quos(warn = TRUE, ...)
  tsum(
    .data, lst_quos$first, remainder = NULL, 
    FUN = function(x) dplyr::summarise_all(x, .funs = .funs)
  )
}

#' @rdname tsummarise
#' @export
tsummarise_if <- function(.data, ..., .predicate, .funs) {
  lst_quos <- separate_quos(warn = TRUE, ...)
  tsum(
    .data, lst_quos$first, remainder = NULL, 
    FUN = function(x) 
      dplyr::summarise_if(x, .predicate = .predicate, .funs = .funs)
  )
}

#' @rdname tsummarise
#' @export
tsummarise_at <- function(.data, ..., .vars, .funs) {
  lst_quos <- separate_quos(warn = TRUE, ...)
  tsum(
    .data, lst_quos$first, remainder = NULL, 
    FUN = function(x) 
      dplyr::summarise_at(x, .vars = .vars, .funs = .funs)
  )
}

#' @rdname tsummarise
#' @export
tsummarize <- tsummarise

#' @rdname tsummarise
#' @export
tsummarize_all <- tsummarise_all

#' @rdname tsummarise
#' @export
tsummarize_if <- tsummarise_if

#' @rdname tsummarise
#' @export
tsummarize_at <- tsummarise_at

tsum <- function(.data, first, remainder = NULL, FUN = summarise) {
  index <- index(.data)
  grps <- groups(.data)

  # check if the index variable is present in the first call
  first_var <- as.character(first_arg(first))
  idx_var <- quo_text2(index)
  if (is_false(has_index_var(j = first_var, x = .data))) {
    abort(sprintf("Can't find `index` (`%s`) in the first name-value pair.", idx_var))
  }
  idx <- quo_text2(index)
  idx_name <- names(first)
  if (idx_name == "") {
    names(first) <- idx_name <- idx
  }
  idx_sym <- sym(idx_name)

  # aggregate over time
  chr_grps <- c(flat_grps <- key_flatten(grps), idx_name) 
  pre_data <- .data %>% 
    ungroup() %>% 
    mutate(!!! first, drop = TRUE)
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
    validate = FALSE, ordered = is_ordered(.data)
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
