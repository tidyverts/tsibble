#' @title Aggregate over calendar periods
#'
#' @description It computes summary statistics for a tsibble over calendar
#'    periods, usually used in combination of [group_by].
#'
#' @param .data A tsibble (of `tbl_ts` class).
#' @param ... Name-value pairs of summary functions. To aggregate tsibble over
#'    a certain calendar period, for example yearly aggregates, `~ year()` needs
#'    passing to `...`. Please see details.
#'
#' @author Earo Wang
#' @rdname tsummarise
#' @details It's S3 method implemented for tsibble() (`tbl_ts`) obtained from
#'    [dplyr::summarise()]. A formula with `~` followed by one of calendar component
#'    functions from base, [lubridate] and [zoo] specifies the period when summary
#'    functions are carried out.  Currently `~ year()` indicates yearly aggregates.
#'    `~ yearqtr()` indicates quarterly aggregates. `~ yearmon()` indicates
#'    monthly aggregates. `~ as_date()` or `as.Date()` indicates daily aggregates.
#' @return A tsibble class when the `~` is present.
#'
#' @examples
#'    # pkgs_ts <- as_tsibble(tidypkgs, index = date, package)
#'    # pkgs_ts %>% 
#'    #   group_by(package) %>% 
#'    # summarise(avg_count = mean(count), month = ~ as.yearmon())
#'
#
tsummarise.tbl_ts <- function(.data, ...) {
  cls <- class(.data)
  grped <- dplyr::is.grouped_df(.data)
  if (grped) grps <- dplyr::groups(.data)
  index <- index(.data)
  dots_cap <- quos(..., .named = TRUE)
  # Find the special formula from a set of quos
  sp_f <- tilde_detect(dots_cap)
  idx <- sp_f$index
  if (is_empty(idx)) { # if there's no ~ in ..., tbl_ts is dropped
    .data <- NextMethod()
    # drop tbl_ts
    return(structure(.data, class = c("tbl_ts", "tbl_df", "data.frame")))
  } else {
    str_time <- sp_f$var_name
    sym_time <- sym(str_time)
    fun <- sp_f$fun
    # check whether fun is in the dictionary
    if (is_false(fun %in% builtin_dict())) {
      abort(paste(fun, "is not supported yet."))
    }
    # using group_by, sometimes it drops class attributes, e.g. as.yearmon
    .data <- .data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!str_time := UQ(sym(fun))(!!index))
    sum_args <- dots_cap[-idx] # used for summarise
    if (grped) {
      .data <- .data %>%
        dplyr::group_by(!!!grps) %>%
        dplyr::group_by(!!sym_time, add = TRUE)
    } else {
      .data <- .data %>%
        dplyr::group_by(!!sym_time)
    }
    .data <- .data %>%
        dplyr::summarise(!!!sum_args)
    attr(.data, "key") <- if (grped) {
      # ToDo: check if grouping vars should be key variables
      purrr::map(grps, as_quosure)
      } else {
        as_quosure()
      }
    attr(.data, "index") <- sym_time
    attr(.data, "interval") <- pull_interval(
      eval_tidy(sym_time, data = .data)
    )
    return(structure(.data, class = cls))
  }
}

tsummarize.tbl_ts <- tsummarise.tbl_ts

tilde_detect <- function(...) { # x be a list of quosures
  dots_names <- names2(quos_auto_name(...))
  strs <- quos(...)
  sp_f <- grepl("^~", strs) # should only length(TRUE) <= 1
  sp_idx <- which(sp_f == TRUE, useNames = FALSE)
  sp_time <- gsub("^~(.*)\\()", "\\1", strs[sp_idx])
  return(list(
    index = sp_idx,
    fun = sp_time,
    var_name = dots_names[sp_idx]
  ))
}

builtin_dict <- function() {
  c("year", "as.yearmon", "as.yearqtr", "as_date", "as.Date")
}


