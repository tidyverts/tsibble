by_row <- function(FUN, .data, ordered = TRUE, interval = NULL, ...) {
  FUN <- match.fun(FUN, descend = FALSE)
  tbl <- FUN(as_tibble(.data), ...)
  update_tsibble(tbl, .data, ordered = ordered, interval = interval)
}

# put new data with existing attributes
update_tsibble <- function(
  new, old, ordered = TRUE, interval = NULL, validate = FALSE
) {
  restore_index_class(build_tsibble(
    new, key = key(old), index = !! index(old), index2 = !! index2(old),
    regular = is_regular(old), ordered = ordered, interval = interval, 
    validate = validate
  ), old)
}

# needed when grouping by index2 (e.g. summarise)
group_by_index2 <- function(x) {
  grps <- groups(x)
  idx2 <- index2(x)
  x <- as_tibble(x)
  group_by(x, !!! flatten(c(grps, idx2)))
}

as_grouped_df <- function(x) {
  class(x) <- class(x)[-match("tbl_ts", class(x))] # remove "tbl_ts"
  class(x)[match("grouped_ts", class(x))] <- "grouped_df"
  x
}

grped_df_by_key <- function(.data) {
  grp <- group_vars(.data)
  key <- key_vars(.data)
  if (all(is.element(key, grp))) {
    as_grouped_df(.data)
  } else {
    grouped_df(as_tibble(.data), key)
  }
}

restore_index_class <- function(new, old) {
  old_idx <- index2(old)
  new_idx <- index2(new)
  class(new[[new_idx]]) <- class(old[[old_idx]])
  if (!identical(interval(new), interval(old))) {
    attr(new, "interval") <- pull_interval(new[[new_idx]])
  }
  new
}

join_tsibble <- function(FUN, x, y, by = NULL, copy = FALSE, validate = FALSE, 
  ...) {
  FUN <- match.fun(FUN, descend = FALSE)
  tbl_x <- as_tibble(x)
  tbl_y <- as_tibble(y)
  tbl <- FUN(x = tbl_x, y = tbl_y, by = by, copy = copy, ...)
  update_tsibble(tbl, x, ordered = is_ordered(x), validate = validate)
}

rename_tsibble <- function(.data, ...) {
  names_dat <- names(.data)
  val_vars <- tidyselect::vars_rename(names_dat, ...)

  # index
  res <- .data %>% 
    rename_index(val_vars) %>% 
    rename_index2(val_vars) %>% 
    rename_key(val_vars) %>% 
    rename_group(val_vars)
  names(res) <- names(val_vars)

  build_tsibble_meta(
    res, key = key(res), index = !! index(res), index2 = !! index2(res),
    regular = is_regular(res), ordered = is_ordered(res), 
    interval = interval(res)
  )
}

select_tsibble <- function(.data, ..., validate = TRUE) {
  quos <- enquos(...)
  sel_vars <- tidyselect::vars_select(names(.data), !!! quos)
  idx_chr <- index_var(.data)
  sel_idx <- idx_chr %in% sel_vars
  if (!sel_idx) { # index isn't selected
    inform(sprintf("Selecting index: \"%s\"", idx_chr))
    val_vars <- c(sel_vars, idx_chr)
  } else {
    val_vars <- sel_vars
  }
  sel_data <- select(as_tibble(.data), !!! val_vars)
  
  # key (key of the reduced size (bf & af) but also different names)
  key_vars <- syms(val_vars[val_vars %in% key_vars(.data)])
  
  if (validate) {
    vec_names <- names(val_vars)
    validate <- !has_all_key(vec_names, .data)
  }

  if (validate) {
    sel_data <- retain_tsibble(sel_data, key_vars, index(.data))
  }
  
  build_tsibble_meta(
    sel_data, key = key_vars, index = !! index(.data), index2 = !! index2(.data),
    regular = is_regular(.data), ordered = is_ordered(.data), 
    interval = interval(.data)
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

assign_index_null <- function(j, x) {
  is_index_null(x)
  index <- c(index_var(x), index2_var(x))
  any(index %in% j)
}
