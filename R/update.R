by_row <- function(FUN, .data, ordered = TRUE, ..., .preserve = FALSE) {
  FUN <- match.fun(FUN, descend = FALSE)
  tbl <- FUN(as_tibble(.data), ..., .preserve = .preserve)
  if (.preserve) {
    update_meta2(tbl, .data, ordered = ordered, interval = interval(.data))
  } else {
    update_meta(tbl, .data, ordered = ordered, interval = interval(.data))
  }
}

# put new data with existing attributes (update key)
update_meta <- function(new, old, ordered = TRUE, interval = TRUE,
                        validate = FALSE) {
  if (validate) {
    retain_tsibble(new, key = key(old), index = index(old))
    validate <- FALSE
  }
  build_tsibble(new,
    key = !!key_vars(old), index = !!index(old), index2 = !!index2(old),
    ordered = ordered, interval = interval, validate = validate,
    .drop = is_key_dropped(old)
  )
}

# preserve key data
update_meta2 <- function(new, old, ordered = TRUE, interval = TRUE,
                         validate = FALSE) {
  old_key <- select(key_data(old), !!!key(old))
  if (is_empty(old_key)) {
    return(update_meta(
      new, old,
      ordered = ordered, interval = interval, validate = validate
    ))
  }
  grped_df <- grouped_df(new, key_vars(old), drop = key_drop_default(old))
  new_key <- right_join(group_data(grped_df), old_key, by = key_vars(old))
  null_lgl <- map_lgl(new_key[[".rows"]], is_null)
  new_key[[".rows"]][null_lgl] <- list(integer())
  build_tsibble(new,
    key_data = new_key, index = !!index(old), index2 = !!index2(old),
    ordered = ordered, interval = interval, validate = validate
  )
}

rename_tsibble <- function(.data, ...) {
  names_dat <- names(.data)
  lst_quos <- enquos(...)
  if (is_empty(lst_quos)) return(.data)

  val_vars <- vars_rename(names_dat, !!!lst_quos)
  old_idx <- index_var(.data)
  old_idx2 <- index2_var(.data)
  old_key <- key_vars(.data)
  old_grp <- group_vars(.data)

  new_names <- names(val_vars)
  new_idx <- new_names[old_idx == val_vars]
  attr(new_idx, "ordered") <- is_ordered(.data)
  new_idx2 <- new_names[old_idx2 == val_vars]
  new_key <- new_names[val_vars %in% old_key]
  new_grp <- new_names[val_vars %in% old_grp]
  attr(.data, "index") <- new_idx
  attr(.data, "index2") <- new_idx2
  names(attr(.data, "key")) <- c(new_key, ".rows")
  if (!is_empty(old_grp)) {
    names(attr(.data, "groups")) <- c(new_grp, ".rows")
  }
  names(.data) <- new_names
  .data
}

select_tsibble <- function(data, ...) {
  sel_vars <- vars_select(names(data), ...)

  key_chr <- key_vars(data)
  sel_key <- sel_vars[vec_in(sel_vars, key_chr)]
  all_key <- all(is.element(key_chr, sel_key))
  if (!all_key) {
    sel_vars <- c(setdiff(key_chr, sel_key), sel_vars)
  }

  idx_chr <- index_var(data)
  sel_idx <- vec_in(idx_chr, sel_vars)
  if (!sel_idx) {
    sel_vars <- c(idx_chr, sel_vars)
  }

  sel_data <- select(as_tibble(data), !!!sel_vars)

  build_tsibble(sel_data,
    key = !!key_chr, key_data = key_data(data),
    index = !!index(data), index2 = !!index2(data),
    ordered = is_ordered(data), interval = interval(data),
    validate = FALSE, .drop = is_key_dropped(data))
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
