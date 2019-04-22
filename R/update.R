by_row <- function(FUN, .data, ordered = TRUE, interval = TRUE, ..., 
  .preserve = FALSE) {
  FUN <- match.fun(FUN, descend = FALSE)
  tbl <- FUN(as_tibble(.data), ..., .preserve = .preserve)
  if (.preserve) {
    update_meta2(tbl, .data, ordered = ordered, interval = interval)
  } else {
    update_meta(tbl, .data, ordered = ordered, interval = interval)
  }
}

# put new data with existing attributes (update key)
update_meta <- function(
  new, old, ordered = TRUE, interval = TRUE, validate = FALSE
) {
  if (validate) {
    retain_tsibble(new, key = key(old), index = index(old))
  }
  validate <- TRUE || validate
  restore_index_class(build_tsibble(
    new, key = !! key_vars(old), index = !! index(old), index2 = !! index2(old),
    ordered = ordered, interval = interval, validate = validate,
    .drop = is_key_dropped(old)
  ), old)
}

# preserve key data
update_meta2 <- function(
  new, old, ordered = TRUE, interval = TRUE, validate = FALSE
) {
  old_key <- select(key_data(old), !!! key(old))
  if (is_empty(old_key)) {
    return(update_meta(
      new, old, ordered = ordered, interval = interval, validate = validate
    ))
  }
  new_key <- group_data(grouped_df(new, key_vars(old))) %>% 
    right_join(old_key, by = key_vars(old))
  null_lgl <- map_lgl(new_key[[".rows"]], is_null)
  new_key[[".rows"]][null_lgl] <- list(integer())
  restore_index_class(build_tsibble(
    new, key_data = new_key, index = !! index(old), index2 = !! index2(old),
    ordered = ordered, interval = interval, validate = validate
  ), old)
}

# needed when grouping by index2, dropping empty groups (e.g. summarise)
group_by_index2 <- function(x) {
  idx2 <- index2(x)
  x <- as_tibble(x)
  group_by(x, !! idx2, add = TRUE, .drop = TRUE)
}

restore_index_class <- function(new, old) {
  old_idx <- index2(old)
  new_idx <- index2(new)
  class(new[[new_idx]]) <- class(old[[old_idx]])
  if (!identical(interval(new), interval(old))) {
    attr(new, "interval") <- interval_pull(new[[new_idx]])
  }
  new
}

rename_tsibble <- function(.data, ...) {
  names_dat <- names(.data)
  lst_quos <- enquos(...)
  if (is_empty(lst_quos)) return(.data)

  val_vars <- vars_rename(names_dat, !!! lst_quos)
  # index
  res <- rename_index2(rename_index(.data, val_vars), val_vars)
  res <- rename_group(rename_key(res, val_vars), val_vars)
  names(res) <- names(val_vars)

  build_tsibble(
    res, key_data = key_data(res),
    index = !! index(res), index2 = !! index2(res),
    ordered = is_ordered(res), interval = interval(res), validate = FALSE
  )
}

select_tsibble <- function(.data, ..., validate = TRUE) {
  sel_data <- select(as_tibble(.data), ...)

  sel_vars <- names(sel_data)
  idx_chr <- index_var(.data)
  sel_idx <- idx_chr %in% sel_vars
  if (!sel_idx) { # index isn't selected
    inform(sprintf("Selecting index: \"%s\"", idx_chr))
    sel_data[[idx_chr]] <- .data[[idx_chr]]
    val_vars <- c(sel_vars, idx_chr)
  } else {
    val_vars <- sel_vars
  }
  
  # key (key of the reduced size (bf & af) but also different names)
  key_vars <- val_vars[val_vars %in% key_vars(.data)]
  
  if (validate) {
    vec_names <- names(val_vars)
    validate <- !has_all_key(vec_names, .data)
  }

  if (validate) {
    sel_data <- retain_tsibble(sel_data, key_vars, index(.data))
  }
  
  build_tsibble(
    sel_data,
    key = !! key_vars, index = !! index(.data), index2 = !! index2(.data),
    ordered = is_ordered(.data), interval = interval(.data),
    validate = FALSE, .drop = is_key_dropped(.data)
  )
}

has_index <- function(j, x) {
  is_index_null(x)
  index <- c(index_var(x), index2_var(x))
  any(index %in% j)
}

has_any_key <- function(j, x) {
  key_vars <- key_vars(x)
  any(key_vars %in% j)
}

has_all_key <- function(j, x) {
  key_vars <- key_vars(x)
  all(key_vars %in% j)
}
