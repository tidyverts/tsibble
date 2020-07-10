globalVariables(c("name"))

pivot_longer.tbl_ts <- function(data, cols, names_to = "name", ...) {
  if (!has_length(names_to)) {
    abort("`pivot_longer(<tsibble>)` can't accept zero-length `names_to`.")
  }
  if (".value" %in% names_to) {
    abort("`pivot_longer(<tsibble>)` can't accept the special \".value\" in `names_to`.")
  }
  new_key <- c(key_vars(data), names_to)
  vars <- names(eval_select(enquo(cols), data))
  data <- mutate_index2(data, vars)
  tbl <- tidyr::pivot_longer(as_tibble(data),
    cols = !!enquo(cols), names_to = names_to, ...)
  build_tsibble(tbl,
    key = !!new_key, index = !!index(data), index2 = !!index2(data),
    ordered = is_ordered(data), interval = interval(data), validate = FALSE
  )
}

pivot_wider.tbl_ts <- function(data, id_cols = NULL, names_from = name, ...) {
  key_var <- vars_pull(names(data), !!enquo(names_from))
  if (has_index(key_var, data)) {
    abort(c(
      sprintf("Column `%s` (index) can't be widened.", key_var),
      i = "Please use `as_tibble()` to coerce."
    ))
  }
  key_left <- setdiff(key_vars(data), key_var)
  new_key <- key_vars(remove_key(data, .vars = key_left))

  tbl <- tidyr::pivot_wider(as_tibble(data),
    id_cols = !!enquo(id_cols), names_from = !!enquo(names_from), ...)
  tbl <- retain_tsibble(tbl, new_key, index(data))

  vars <- names(tbl)
  data <- mutate_index2(data, vars)
  build_tsibble(
    tbl,
    key = !!new_key, index = !!index(data), index2 = !!index2(data),
    ordered = is_ordered(data), interval = is_regular(data),
    validate = FALSE
  )
}

gather.tbl_ts <- function(data, key = "key", value = "value", ...,
                          na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  key <- as_string(enexpr(key))
  new_key <- c(key_vars(data), key)
  value <- enexpr(value)
  exprs <- enexprs(...)
  if (is_empty(exprs)) {
    exprs <- setdiff(
      names(data),
      c(quo_name(key), quo_name(value), quo_name(index(data)))
    )
  }
  vars <- names(eval_select(expr(c(...)), data))
  data <- mutate_index2(data, vars)
  tbl <- tidyr::gather(
    as_tibble(data),
    key = !!key, value = !!value, !!!exprs,
    na.rm = na.rm, convert = convert, factor_key = factor_key
  )
  build_tsibble(tbl,
    key = !!new_key, index = !!index(data), index2 = !!index2(data),
    ordered = is_ordered(data), interval = interval(data), validate = FALSE
  )
}

spread.tbl_ts <- function(data, key, value, ...) {
  key <- enexpr(key)
  value <- enexpr(value)
  key_var <- vars_pull(names(data), !!key)
  if (has_index(key_var, data)) {
    abort(c(
      sprintf("Column `%s` (index) can't be spread.", key_var),
      i = "Please use `as_tibble()` to coerce."
    ))
  }
  key_left <- setdiff(key_vars(data), key_var)
  new_key <- key_vars(remove_key(data, .vars = key_left))

  tbl <- tidyr::spread(as_tibble(data), key = !!key, value = !!value, ...)
  tbl <- retain_tsibble(tbl, new_key, index(data))

  vars <- names(tbl)
  data <- mutate_index2(data, vars)
  build_tsibble(
    tbl,
    key = !!new_key, index = !!index(data), index2 = !!index2(data),
    ordered = is_ordered(data), interval = is_regular(data),
    validate = FALSE
  )
}

nest.tbl_ts <- function(.data, ...) {
  data <- .data
  tbl_nest <- tidyr::nest(as_tibble(data), ...)
  data_names <- names(data)
  nest_names <- names(tbl_nest)
  nest_vars <- setdiff(data_names, nest_names)
  if (!has_all_key(nest_vars, data) && !has_index(nest_vars, data)) {
    build_tsibble(tbl_nest,
      key = setdiff(key_vars(data), nest_vars),
      index = !!index(data), validate = FALSE
    )
  } else if (!has_index(nest_vars, data)) {
    build_tsibble(tbl_nest, index = !!index(data), validate = FALSE)
  } else {
    new_lst <- nest_names[map_lgl(tbl_nest, is_list)]
    old_lst <- data_names[map_lgl(data, is_list)]
    lst_vars <- setdiff(new_lst, old_lst)
    data <- remove_key(ungroup(data), nest_vars)
    tbl_nest[[lst_vars]] <- lapply(tbl_nest[[lst_vars]],
      function(x) update_meta(x, data))
    tbl_nest
  }
}

nest.grouped_ts <- nest.tbl_ts

unnest.tbl_ts <- function(data, ...) {
  data <- as_tibble(data)
  NextMethod()
}

#' Unnest a data frame consisting of tsibbles to a tsibble
#'
#' @description
#' \lifecycle{deprecated}
#'
#' @param data A data frame contains homogenous tsibbles in the list-columns.
#' @param cols Names of columns to unnest.
#' @inheritParams as_tsibble
#' @keywords internal
#' @export
unnest_tsibble <- function(data, cols, key = NULL, validate = TRUE) {
  if (!is_installed("tidyr") && utils::packageVersion("tidyr") >= "0.9.0") {
    abort("Package 'tidyr' (>= v1.0.0) required for `unnest_tsibble()`.")
  }
  cols <- enquo(cols)
  if (quo_is_missing(cols)) {
    abort("Argument `cols` for columns to unnest is required.")
  }
  unnested_data <- tidyr::unnest(as_tibble(data), cols = !!cols)

  if (is_tsibble(data)) {
    idx <- index(data)
    tsbl <- data
  } else {
    data_names <- names(data)
    unnested_names <- names(unnested_data)
    new_lst <- unnested_names[map_lgl(unnested_data, is_list)]
    old_lst <- data_names[map_lgl(data, is_list)]
    lst_cols <- setdiff(old_lst, new_lst)
    # checking if the nested columns has `tbl_ts` class
    tsbl_col <- map_lgl(data[lst_cols], validate_list_of_tsibble)
    if (sum(tsbl_col) == 0) {
      abort("Unnested columns contain no tsibble columns.")
    }
    first_nested <- data[lst_cols][1L, ]
    eval_col <- map(first_nested, first)

    tsbl <- eval_col[tsbl_col][[1L]]
    idx <- index(tsbl)
  }

  key <- names(eval_select(enquo(key), data = unnested_data))
  idx_chr <- as_string(idx)
  class(unnested_data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(
    unnested_data,
    key = !!key, index = !!idx, index2 = !!index2(tsbl),
    ordered = is_ordered(tsbl), interval = is_regular(tsbl), validate = validate
  )
}

validate_list_of_tsibble <- function(x) {
  all(vapply(x, function(x) is_tsibble(x), logical(1)))
}

fill.tbl_ts <- function(data, ..., .direction = c("down", "up")) {
  res <- NextMethod()
  update_meta2(res, data, ordered = is_ordered(data), interval = interval(data))
}

fill.grouped_ts <- fill.tbl_ts

drop_na.tbl_ts <- function(data, ...) {
  res <- NextMethod()
  update_meta2(res, data, ordered = is_ordered(data), interval = interval(data))
}

drop_na.grouped_ts <- drop_na.tbl_ts
