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
    groups = groups(old), regular = is_regular(old), validate = validate,
    ordered = ordered, interval = interval
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
  old_idx <- as_string(index(old))
  new_idx <- as_string(index(new))
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

tsibble_rename <- function(.data, ...) {
  names_dat <- names(.data)
  val_vars <- tidyselect::vars_rename(names_dat, ...)

  # index
  idx <- index_rename(.data, val_vars)
  # index2
  idx2 <- index2_rename(.data, val_vars)
  attr(.data, "index2") <- idx2
  # key (key of the same size (bf & af))
  new_key <- key_rename(.data, val_vars)
  # groups
  new_grp <- grp_rename(.data, val_vars)

  names(.data) <- names(val_vars)
  build_tsibble_meta(
    .data, key = new_key, index = !! idx, index2 = !! idx2,
    groups = new_grp, regular = is_regular(.data),
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

tsibble_select <- function(.data, ..., validate = TRUE) {
  dots <- c(enexprs(...), index(.data))
  names_dat <- names(.data)
  val_vars <- tidyselect::vars_select(names_dat, !!! dots)
  sel_data <- select(as_tibble(.data), !!! val_vars)
  
  # index
  idx <- index_rename(.data, val_vars)
  # index2
  idx2 <- index2_rename(.data, val_vars)
  # key (key of the reduced size (bf & af) but also different names)
  key_vars <- val_vars[val_vars %in% key_vars(.data)]
  tmp_data <- key_remove(.data, key_vars, validate = FALSE)
  new_key <- key_rename(tmp_data, key_vars)
  # groups
  new_grp <- grp_rename(.data, val_vars)
  
  if (validate) {
    vec_names <- union(names(val_vars), names(.data))
    # either key or index is present in ...
    # suggests that the operations are done on these variables
    # validate = TRUE to check if tsibble still holds
    # val_idx <- has_index(vec_names, .data)
    # val_key <- has_any_key(vec_names, .data)
    # validate <- val_idx || val_key
    validate <- has_any_key(vec_names, .data)
  }
  
  build_tsibble(
    sel_data, key = new_key, index = !! idx, index2 = !! idx2,
    groups = new_grp, regular = is_regular(.data), validate = validate, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

validate_vars <- function(j, x) { # j = quos/chr/dbl
  tidyselect::vars_select(.vars = x, !!! j)
}

# this function usually follows validate_vars()
has_index <- function(j, x) {
  is_index_null(x)
  index <- c(quo_name(index(x)), quo_name(index2(x)))
  any(index %in% j)
}

has_distinct_key <- function(j, x) {
  key_vars <- key_flatten(key_distinct(key(x)))
  all(key_vars %in% j)
}

has_any_key <- function(j, x) {
  key_vars <- key_flatten(key(x))
  any(key_vars %in% j)
}
