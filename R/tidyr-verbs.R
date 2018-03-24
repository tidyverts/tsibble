#' @export
fill.grouped_ts <- function(data, ..., .direction = c("down", "up")) {
  dat <- do(data, fill(., ..., .direction = .direction))
  tsbl <- update_tsibble(dat, data)
  restore_index_class(data, tsbl)
}

complete.tbl_ts <- function(data, ..., fill = list()) {
  comp_data <- NextMethod()
  if (is_grouped_ts(data)) {
    comp_data <- grouped_df(comp_data, vars = key_flatten(groups(data)))
  }
  update_tsibble(comp_data, data)
}

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
    select(!! idx_var, key, spread_val, drop = TRUE)
  compact_tsbl %>%
    tidyr::spread(key = key, value = spread_val, fill = fill) %>%
    arrange(!! idx_var)
}
