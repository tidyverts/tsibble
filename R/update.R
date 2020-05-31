# put new data with existing attributes (update key)
update_meta <- function(new, old, ordered = TRUE, interval = TRUE,
                        validate = FALSE) {
  if (validate) {
    retain_tsibble(new, key = key_vars(old), index = index(old))
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
  new_key <- left_join(old_key, group_data(grped_df), by = key_vars(old))
  null_lgl <- map_lgl(new_key[[".rows"]], is_null)
  new_key[[".rows"]][null_lgl] <- list_of(integer())
  build_tsibble(new,
    key_data = new_key, index = !!index(old), index2 = !!index2(old),
    ordered = ordered, interval = interval, validate = validate
  )
}

#' @export
`names<-.tbl_ts` <- function(x, value) {
  data <- as_tibble(x)
  names(data) <- value
  x_names <- names(x)

  idx <- index_var(x)
  idx_loc <- match(intersect(idx, x_names), x_names)
  idx_name <- value[idx_loc]
  
  idx2 <- index2_var(x)
  idx2_loc <- match(intersect(idx2, x_names), x_names)
  idx2_name <- value[idx2_loc]

  key <- key_data(x)
  key_loc <- match(intersect(names(key), x_names), x_names)
  key_names <- c(value[key_loc], ".rows")
  if (!identical(key_names, names(key))) {
    names(key) <- c(value[key_loc], ".rows")
  }

  if (is_grouped_ts(x)) {
    groups <- group_data(x)
    group_loc <- match(intersect(names(groups), x_names), x_names)
    group_names <- c(value[group_loc], ".rows")
    if (!identical(group_names, names(groups))) {
      names(groups) <- c(value[group_loc], ".rows")
    }
  }

  build_tsibble_meta(data,
    key_data = key,
    index = idx_name, index2 = idx2_name, ordered = is_ordered(x),
    interval = interval(x)
  )
}

#' @export
`names<-.grouped_ts` <- `names<-.tbl_ts`

bind_tsibble <- function(data, template, position = c("before", "after")) {
  data <- as_tibble(data)
  data_cols <- names(data)
  key_vars <- setdiff(key_vars(template), data_cols)
  key_data <- select(key_data(template), key_vars)
  if (vec_size(key_data) == 1) {
    template <- remove_key(template, setdiff(key_vars(template), key_vars))
  }
  tsbl_vars <- setdiff(c(index_var(template), key_vars(template)), data_cols)
  if (position == "before") {
    res <- bind_cols(as_tibble(template)[tsbl_vars], data)
  } else {
    res <- bind_cols(data, template[tsbl_vars])
  }
  update_meta(res, template,
    ordered = is_ordered(template), interval = interval(template))
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
