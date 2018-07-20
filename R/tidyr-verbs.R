#' @inheritParams tidyr::gather
#'
#' @rdname tidyverse
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
  build_tsibble_meta(
    tbl, key = new_key, index = !! index(data), 
    index2 = !! index2_update(data, vars),
    groups = grp_update(data, vars), regular = is_regular(data), 
    ordered = is_ordered(data), interval = interval(data)
  )
}

#' @inheritParams tidyr::spread
#' @rdname tidyverse
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
  build_tsibble_meta(
    tbl, key = new_key, index = !! index(data), 
    index2 = !! index2_update(data, vars), groups = grp_update(data, vars),
    regular = is_regular(data), ordered = is_ordered(data),
    interval = interval(data)
  )
}

#' @inheritParams tidyr::nest
#' @rdname tidyverse
#' @export
#' @examples
#' pedestrian %>% 
#'   nest(-Sensor)
#' pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   nest()
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
    abort("`nest.tbl_ts()` must nest the `index` in the list-column.")
  }
  tbl <- as_tibble(data)
  if (is_grouped_ts(data)) {
    grp_vars <- group_vars(tbl)
  } else {
    grp_vars <- setdiff(cn, nest_vars)
  }
  data <- ungroup(data)
  if (is_empty(grp_vars)) {
    return(as_lst_ts(tibble::tibble(!! key_var := list(data))))
  }
  nest_vars <- setdiff(nest_vars, grp_vars)
  grp <- syms(grp_vars)
  nest_df <- split_by(data, !!! grp)
  out <- distinct(data, !!! grp)
  out[[key_var]] <- purrr::map(
    nest_df, ~ tsibble_select(., !!! nest_vars, validate = FALSE)
  )
  as_lst_ts(out)
}

#' @param key Unquoted variables to create the key (via [id]) after unnesting.
#' @inheritParams tidyr::unnest
#'
#' @rdname tidyverse
#' @export
#' @examples
#' nested_ped <- pedestrian %>% 
#'   nest(-Sensor)
#' nested_ped %>% 
#'   unnest(key = id(Sensor))
#' nested_tourism <- tourism %>% 
#'   nest(-Region, -State)
#' nested_tourism %>% 
#'   unnest(key = id(Region | State))
unnest.lst_ts <- function(data, ..., key = id(),
  .drop = NA, .id = NULL, .sep = NULL, .preserve = NULL
) {
  key <- use_id(data, !! enquo(key))
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

  # checking if the nested columns has `tbl_ts` class (only for the first row)
  first_nested <- slice(nested, 1)
  eval_df <- purrr::imap(first_nested, dplyr::first)
  is_tsbl <- purrr::map_lgl(eval_df, is_tsibble)
  if (is_false(any(is_tsbl))) {
    return(NextMethod())
  }
  if (sum(is_tsbl) > 1) {
    abort("Only accepts a list-column of `tbl_ts` to be unnested.")
  }
  out <- as_tibble(data) %>% 
    unnest(!!! quos, .drop = .drop, .id = .id, .sep = .sep, .preserve = .preserve)
  tsbl <- eval_df[[is_tsbl]]
  idx <- index(tsbl)
  validate <- FALSE
  if (is_empty(key)) {
    validate <- TRUE
  }
  key <- c(key(tsbl), key)
  idx_chr <- quo_text(idx)
  # restore the index class, as it's dropped by NextMethod()
  class(out[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(
    out, key = key, index = !! idx, validate = validate, 
    regular = is_regular(tsbl), interval = interval(tsbl)
  )
}

#' @export
mutate.lst_ts <- function(.data, ...) {
  as_lst_ts(NextMethod())
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

#' @export
left_join.lst_ts <- function(
  x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...
) {
  as_lst_ts(NextMethod())
}

#' @export
right_join.lst_ts <- left_join.lst_ts

#' @export
full_join.lst_ts <- left_join.lst_ts

#' @export
inner_join.lst_ts <- left_join.lst_ts

#' @export
anti_join.lst_ts <- function(x, y, by = NULL, copy = FALSE, ...) {
  as_lst_ts(NextMethod())
}

#' @export
semi_join.lst_ts <- anti_join.lst_ts

as_lst_ts <- function(x) {
  grped_df <- dplyr::is_grouped_df(x)
  if (grped_df) {
    return(structure(
      x, class = c("lst_ts", "grouped_df", "tbl_df", "tbl", "data.frame")
    ))
  }
  structure(x, class = c("lst_ts", "tbl_df", "tbl", "data.frame"))
}
