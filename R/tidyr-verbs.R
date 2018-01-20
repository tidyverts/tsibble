#' @export
fill.grouped_ts <- function(data, ..., .direction = c("down", "up")) {
  dat <- do(data, fill(., ..., .direction = .direction))
  tsbl <- as_tsibble(
    dat, key = key(data), index = !! index(data), groups = groups(data),
    validate = FALSE
  )
  restore_index_class(data, tsbl)
}

complete.tbl_ts <- function(data, ..., fill = list()) {
  grps <- groups(data)
  comp_data <- NextMethod()
  if (is_grouped_ts(data)) {
    comp_data <- grouped_df(comp_data, vars = flatten_key(grps))
  }
  as_tsibble(
    comp_data, key = key(data), index = !! index(data), groups = grps,
    validate = FALSE, regular = is_regular(data)
  )
}

spread_tsbl <- function(data, value, fill = NA, sep = "") {
  spread_val <- measured_vars(data)
  str_val <- paste_comma(spread_val)
  msg <- sprintf("Which one is the 'value' variable: %s?", str_val)
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
  if (is_empty(spread_key)) {
    return(as_tibble(data))
  }
  idx_var <- index(data)
  compact_tsbl <- data %>%
    mutate(key = paste(!!! spread_key, sep = sep)) %>%
    select(!! idx_var, key, spread_val, drop = TRUE)
  compact_tsbl %>%
    tidyr::spread(key = key, value = spread_val, fill = fill) %>%
    arrange(!! idx_var)
}
