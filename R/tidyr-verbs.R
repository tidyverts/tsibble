#' Gather columns into key-value pairs.
#'
#' @param data A `tbl_ts`.
#' @inheritParams tidyr::gather
#'
#' @seealso [tidyr::gather]
#' @rdname gather
#' @export
#' @examples
#' # example from tidyr
#' stocks <- tsibble(
#'   time = as.Date('2009-01-01') + 0:9,
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4)
#' )
#' stocks %>% gather(stock, price, -time)
gather.tbl_ts <- function(data, key = "key", value = "value", ...,
  na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  key <- enexpr(key)
  new_key <- c(key(data), key)
  value <- enexpr(value)
  quos <- enquos(...)
  if (is_empty(quos)) {
    quos <- setdiff(names(data), 
      c(quo_name(key), quo_name(value), quo_name(index(data)))
    )
  }
  vars <- validate_vars(quos, names(data))
  tbl <- gather(
    as_tibble(data), key = !! key, value = !! value, !!! quos,
    na.rm = na.rm, convert = convert, factor_key = factor_key
  )
  build_tsibble(
    tbl, key = new_key, index = !! index(data), 
    index2 = !! index2_update(data, vars),
    groups = grp_update(data, vars), regular = is_regular(data), 
    validate = FALSE, ordered = is_ordered(data), interval = interval(data)
  )
}

#' Spread a key-value pair across multiple columns.
#'
#' @param data A `tbl_ts`.
#' @inheritParams tidyr::spread
#'
#' @seealso [tidyr::spread]
#' @rdname spread
#' @export
#' @examples
#' # example from tidyr
#' stocks <- tsibble(
#'   time = as.Date('2009-01-01') + 0:9,
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4)
#' )
#' stocksm <- stocks %>% gather(stock, price, -time)
#' stocksm %>% spread(stock, price)
#' @export
spread.tbl_ts <- function(data, key, value, fill = NA, convert = FALSE,
  drop = TRUE, sep = NULL) {
  key <- enexpr(key)
  value <- enexpr(value)
  key_var <- tidyselect::vars_pull(names(data), !! key)
  if (has_index(key_var, data)) {
    abort(sprintf("`key` must not be `%s`, as it's the `index`.", key_var))
  }
  key_left <- setdiff(key_vars(data), key_var)
  new_key <- key(key_reduce(data, .vars = key_left, validate = FALSE))

  tbl <- spread(
    as_tibble(data), key = !! key, value = !! value, fill = fill, 
    convert = convert, drop = drop, sep = sep
  )
  vars <- names(tbl)
  build_tsibble(
    tbl, key = new_key, index = !! index(data), 
    index2 = !! index2_update(data, vars), groups = grp_update(data, vars),
    regular = is_regular(data), validate = FALSE, ordered = is_ordered(data),
    interval = interval(data)
  )
}

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
    abort("`nest.tbl_ts()` must nest the `index` in the list columns.")
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
  tibble::new_tibble(out, subclass = "lst_ts")
}

#' @export
unnest.lst_ts <- function(data, ..., .with,
  .drop = NA, .id = NULL, .sep = NULL, .preserve = NULL
) {
  use_id(.with)
  preserve <- tidyselect::vars_select(names(data), !!! enquo(.preserve))
  quos <- enquos(...)
  if (is_empty(quos)) {
    list_cols <- names(data)[purrr::map_lgl(data, is_list)]
    list_cols <- setdiff(list_cols, preserve)
    quos <- syms(list_cols)
  }
  if (length(quos) == 0) {
    return(data)
  }
  nested <- transmute(ungroup(data), !!! quos)
  first_nested <- slice(nested, 1)
  eval_df <- purrr::imap(first_nested, dplyr::first)
  is_tsbl <- purrr::map_lgl(eval_df, is_tsibble)
  if (is_false(any(is_tsbl))) {
    abort("Must contain a list column of tsibble to be unnested.")
  }
  if (sum(is_tsbl) > 1) {
    abort("Only accept only one list column of tsibble to be unnested.")
  }
  out <- as_tibble(data) %>% 
    unnest(!!! quos, .drop = .drop, .id = .id, .sep = .sep, .preserve = .preserve)
  tsbl <- eval_df[[is_tsbl]]
  idx <- index(tsbl)
  key <- c(key(tsbl), .with)
  idx_chr <- quo_text(idx)
  # restore the index class, as it's dropped by NextMethod()
  class(out[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(
    out, key = key, index = !! idx, validate = FALSE, 
    regular = is_regular(tsbl), interval = interval(tsbl)
  )
}

#' @export
mutate.lst_ts <- function(.data, ...) {
  tibble::new_tibble(NextMethod(), subclass = "lst_ts")
}

#' @export
transmute.lst_ts <- mutate.lst_ts

#' @export
select.lst_ts <- mutate.lst_ts

#' @export
rename.lst_ts <- mutate.lst_ts

#' @export
arrange.lst_ts <- mutate.lst_ts

#' @export
filter.lst_ts <- mutate.lst_ts

#' @export
slice.lst_ts <- mutate.lst_ts

#' @export
group_by.lst_ts <- mutate.lst_ts
