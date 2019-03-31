#' @importFrom tidyr gather
#' @export
tidyr::gather

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
#' (stocksm <- stocks %>% gather(stock, price, -time))
#' stocksm %>% spread(stock, price)
gather.tbl_ts <- function(data, key = "key", value = "value", ...,
  na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  key <- sym(enexpr(key))
  new_key <- c(key(data), key)
  value <- enexpr(value)
  exprs <- enexprs(...)
  if (is_empty(exprs)) {
    exprs <- setdiff(names(data), 
      c(quo_name(key), quo_name(value), quo_name(index(data)))
    )
  }
  vars <- vars_select(names(data), !!! exprs)
  data <- mutate_index2(data, vars)
  tbl <- gather(
    as_tibble(data), key = !! key, value = !! value, !!! exprs,
    na.rm = na.rm, convert = convert, factor_key = factor_key
  )
  build_tsibble(
    tbl, key = !! new_key, index = !! index(data), index2 = !! index2(data), 
    regular = is_regular(data), ordered = is_ordered(data), 
    interval = interval(data), validate = FALSE
  )
}

#' @importFrom tidyr spread
#' @export
tidyr::spread

#' @inheritParams tidyr::spread
#' @rdname tidyverse
#' @export
spread.tbl_ts <- function(data, key, value, fill = NA, convert = FALSE,
  drop = TRUE, sep = NULL) {
  key <- enexpr(key)
  value <- enexpr(value)
  key_var <- vars_pull(names(data), !! key)
  if (has_index(key_var, data)) {
    abort(sprintf(
      "Column `%s` (index) can't be spread.\nPlease use `as_tibble()` to coerce.", 
      key_var
    ))
  }
  key_left <- setdiff(key_vars(data), key_var)
  new_key <- key(remove_key(data, .vars = key_left))

  tbl <- spread(
    as_tibble(data), key = !! key, value = !! value, fill = fill, 
    convert = convert, drop = drop, sep = sep
  )
  tbl <- retain_tsibble(tbl, new_key, index(data))

  vars <- names(tbl)
  data <- mutate_index2(data, vars)
  build_tsibble(
    tbl, key = !! new_key, index = !! index(data), index2 = !! index2(data), 
    regular = is_regular(data), ordered = is_ordered(data), interval = NULL, 
    validate = FALSE
  )
}

#' @importFrom tidyr nest
#' @export
tidyr::nest

#' @inheritParams tidyr::nest
#' @rdname tidyverse
#' @export
#' @examples
#' nested_stock <- stocksm %>% 
#'   nest(-stock)
#' stocksm %>% 
#'   group_by(stock) %>% 
#'   nest()
nest.tbl_ts <- function(data, ..., .key = "data") {
  nest_exprs <- enexprs(...)
  key_var <- expr_name(enexpr(.key))
  cn <- names(data)
  if (is_empty(nest_exprs)) {
    nest_vars <- cn
  } else {
    nest_vars <- vars_select(cn, !!! nest_exprs)
  }
  if (is_false(has_index(nest_vars, data))) {
    abort(sprintf(
      "Column `%s` (index) must be nested in the list-column", 
      index_var(data)
    ))
  }
  tbl <- as_tibble(data)
  if (is_grouped_ts(data)) {
    grp_vars <- group_vars(tbl)
  } else {
    grp_vars <- setdiff(cn, nest_vars)
  }
  data <- ungroup(data)
  if (is_empty(grp_vars)) {
    return(as_lst_ts(tibble(!! key_var := list(data))))
  }
  nest_vars <- setdiff(nest_vars, grp_vars)
  grp <- syms(grp_vars)

  out <- select(ungroup(tbl), !!! grp)
  idx <- dplyr::group_indices(data, !!! grp, .drop = TRUE)
  representatives <- which(!duplicated(idx))
  out <- slice(out, representatives)
  tsb_sel <- select_tsibble(data, !!! nest_vars, validate = FALSE)
  out[[key_var]] <- unname(split(tsb_sel, idx))[unique(idx)]
  as_lst_ts(out)
}

#' @importFrom tidyr unnest
#' @export
tidyr::unnest

#' @param key Unquoted variables to create the key (via [id]) after unnesting.
#' @inheritParams tidyr::unnest
#'
#' @rdname tidyverse
#' @export
#' @examples
#' nested_stock %>% 
#'   unnest(key = stock)
unnest.lst_ts <- function(data, ..., key = NULL,
  .drop = NA, .id = NULL, .sep = NULL, .preserve = NULL
) {
  key <- use_id(data, !! enquo(key))
  preserve <- vars_select(names(data), !! enquo(.preserve))
  exprs <- enquos(...)
  if (is_empty(exprs)) {
    list_cols <- names(data)[purrr::map_lgl(data, is_list)]
    list_cols <- setdiff(list_cols, preserve)
    exprs <- syms(list_cols)
  }
  if (length(exprs) == 0) return(data)

  nested <- transmute(ungroup(data), !!! exprs)

  # checking if the nested columns has `tbl_ts` class (only for the first row)
  first_nested <- nested[1L, ]
  eval_df <- purrr::imap(first_nested, dplyr::first)
  is_tsbl <- purrr::map_lgl(eval_df, is_tsibble)
  if (is_false(any(is_tsbl))) return(NextMethod())

  if (sum(is_tsbl) > 1) {
    abort("Only accepts a list-column of `tbl_ts` to be unnested.")
  }
  out <- 
    unnest(
      as_tibble(data), 
      !!! exprs, .drop = .drop, .id = .id, .sep = .sep, .preserve = .preserve
    )
  tsbl <- eval_df[is_tsbl][[1L]]
  idx <- index(tsbl)
  key <- c(key(tsbl), key)
  out <- unnest_tsibble(out, key, idx)

  idx_chr <- as_string(idx)
  # restore the index class, as it's dropped by NextMethod()
  class(out[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(
    out, key = !! key, index = !! idx, index2 = !! index2(tsbl),
    ordered = is_ordered(tsbl), regular = is_regular(tsbl), 
    interval = NULL, validate = FALSE
  )
}

#' @rdname tidyverse
#' @export
#' @examples
#' stock_qtl <- stocksm %>% 
#'   group_by(stock) %>% 
#'   index_by(day3 = lubridate::floor_date(time, unit = "3 day")) %>% 
#'   summarise(
#'     value = list(quantile(price)), 
#'     qtl = list(c("0%", "25%", "50%", "75%", "100%"))
#'   )
#' unnest(stock_qtl, key = qtl)
unnest.tbl_ts <- function(data, ..., key = NULL,
  .drop = NA, .id = NULL, .sep = NULL, .preserve = NULL
) {
  key <- use_id(data, !! enquo(key))
  tbl <- 
    unnest(
      as_tibble(data), 
      ..., .drop = .drop, .id = .id, .sep = .sep, .preserve = .preserve
    )
  key <- c(key(data), key)
  idx <- index(data)
  tbl <- unnest_tsibble(tbl, key, idx)

  idx_chr <- as_string(idx)
  class(tbl[[idx_chr]]) <- class(data[[idx_chr]])
  build_tsibble(
    tbl, key = !! key, index = !! idx, index2 = !! index2(data), 
    ordered = is_ordered(data), regular = is_regular(data), 
    interval = NULL, validate = FALSE
  )
}

# used for unnest() to check if the tsibble holds
unnest_tsibble <- function(data, key, index) {
  is_dup <- duplicated_key_index(data, key, index)
  if (is_dup) {
    header <- "The result is not a valid tsibble.\n"
    hint <- "Do you forget argument `key` in `unnest()` to create the key?."
    abort(paste0(header, hint))
  }
  data
}

#' @importFrom tidyr fill
#' @export
tidyr::fill

#' @inheritParams tidyr::fill
#' @rdname tidyverse
#' @export
fill.tbl_ts <- function(data, ..., .direction = c("down", "up")) {
  res <- NextMethod()
  update_meta2(res, data, ordered = is_ordered(data), interval = interval(data))
}

#' @rdname tidyverse
#' @export
fill.grouped_ts <- fill.tbl_ts

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
  cls <- c("tbl_df", "tbl", "data.frame")
  grped_df <- dplyr::is_grouped_df(x)
  if (grped_df) {
    class(x) <- c("lst_ts", "grouped_df", cls)
  } else {
    class(x) <- c("lst_ts", cls)
  }
  x
}
