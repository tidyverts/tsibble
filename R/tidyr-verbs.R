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
# pedestrian %>% 
#   # group_by(Sensor) %>% 
#   tidyr::nest(-Sensor)

#' @export
nest.tbl_ts <- function(data, ..., .key = "data") {
  nest_quos <- enquos(...)
  key_var <- quo_name(enexpr(.key))
  cn <- names(data)
  if (is_empty(nest_quos)) {
    nest_vars <- cn
  }
  nest_vars <- validate_vars(nest_quos, cn)
  if (is_false(has_index(nest_vars, data))) {
    abort("`nest.tbl_ts()` must have the `index` in the nested data.")
  }
  if (is_grouped_ts(data)) {
    grp_vars <- group_vars(data)
  } else {
    grp_vars <- setdiff(cn, nest_vars)
  }
  data <- ungroup(data)
  if (is_empty(grp_vars)) {
    return(tibble(!! key_var := list(data)))
  }
  nest_vars <- setdiff(nest_vars, grp_vars)
  grp <- syms(grp_vars)
  nest_df <- split_by(data, !!! grp)
  out <- purrr::map_dfr(nest_df, ~ distinct(., !!! grp))
  out[[key_var]] <- purrr::map(
    nest_df, ~ tsibble_select(., !!! nest_vars, validate = FALSE)
  )
  tibble::new_tibble(out, subclass = "lst_ts")
}
