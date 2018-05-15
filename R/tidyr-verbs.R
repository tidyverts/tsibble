spread_tsbl <- function(data, value, fill = NA, sep = "") {
  spread_val <- measured_vars(data)
  str_val <- paste_comma(surround(spread_val, "`"))
  msg <- sprintf("Can't determine the `value`: %s.", str_val)
  if (quo_is_missing(value)) {
    if (is_false(has_length(spread_val, 1))) {
      abort(msg)
    }
  } else {
    val <- quo_text(value)
    if (is_false(val %in% spread_val)) {
      abort(msg)
    }
    spread_val <- val
  }
  # ToDo: only works with a single key rather than the nested and grouped keys
  spread_key <- key(data)
  idx_var <- index(data)
  if (is_empty(spread_key)) {
    return(as_tibble(data %>% arrange(!! idx_var)))
  }
  compact_tsbl <- data %>%
    mutate(key = paste(!!! spread_key, sep = sep)) %>%
    select(!! idx_var, key, spread_val, .drop = TRUE)
  compact_tsbl %>%
    tidyr::spread(key = key, value = spread_val, fill = fill) %>%
    arrange(!! idx_var)
}

# library(rlang)
# lst_ped <- pedestrian %>% 
#   # group_by(Sensor) %>% 
#   tidyr::nest(-Sensor)
# lst_ped %>% 
#   tidyr::unnest()
# lst_ped2 <- pedestrian %>% 
#   index_by(yrmth = yearmonth(Date)) %>% 
#   group_by(Sensor) %>% 
#   tidyr::nest()
# lst_ped2 %>% 
#   tidyr::unnest()
# pedestrian %>% 
#   group_by(Sensor) %>% 
#   tidyr::nest()
# tidyr::unnest(lst_ped)
# lst_t <- tourism %>% 
#   group_by(Purpose) %>% 
#   tidyr::nest()
#
# lst_t %>% 
#   tidyr::unnest()

#' @export
nest.tbl_ts <- function(data, ..., .key = "data") {
  nest_quos <- enquos(...)
  key_var <- quo_name(enexpr(.key))
  cn <- names(data)
  if (is_empty(nest_quos)) {
    nest_vars <- cn
  } else {
    nest_vars <- tidyselect::vars_select(cn, !!! nest_quos)
  }
  if (is_false(has_index(nest_vars, data))) {
    abort("`nest.tbl_ts()` must have the `index` in the list of data columns.")
  }
  tbl <- as_tibble(data)
  if (dplyr::is_grouped_df(tbl)) {
    grp_vars <- group_vars(tbl)
  } else {
    grp_vars <- setdiff(cn, nest_vars)
  }
  data <- ungroup(data)
  if (is_empty(grp_vars)) {
    return(tibble::tibble(!! key_var := list(data)))
  }
  nest_vars <- setdiff(nest_vars, grp_vars)
  grp <- syms(grp_vars)
  nest_df <- split_by(data, !!! grp)
  out <- distinct(data, !!! grp)
  out[[key_var]] <- purrr::map(
    nest_df, ~ tsibble_select(., !!! nest_vars, validate = FALSE)
  )
  idx <- index(data)
  tibble::new_tibble(
    out, 
    key = key(data), 
    index = idx, 
    regular = is_regular(data),
    # dark: work around for unnest(), since it drops the index class
    index_class = class(eval_tidy(idx, data)), 
    subclass = "lst_ts"
  )
}

#' @export
unnest.lst_ts <- function(data, ..., 
  .drop = NA, .id = NULL, .sep = NULL, .preserve = NULL
) {
  out <- NextMethod()
  idx <- index(data)
  idx_chr <- quo_text(idx)
  # restore the index class, as it's dropped by NextMethod()
  class(out[[idx]]) <- attr(data, "index_class")
  build_tsibble(
    out, key = key(data), index = !! idx, validate = FALSE, 
    regular = is_regular(data),
  )
}
