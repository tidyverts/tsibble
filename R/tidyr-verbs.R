#' @inheritParams tidyr::gather
#'
#' @rdname tsibble-tidyverse
#' @examples
#' library(tidyr)
#' # example from tidyr
#' stocks <- tsibble(
#'   time = as.Date('2009-01-01') + 0:9,
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4)
#' )
#' (stocksm <- stocks %>% gather(stock, price, -time))
#' stocksm %>% spread(stock, price)
#' @export
gather.tbl_ts <- function(data, key = "key", value = "value", ...,
  na.rm = FALSE, convert = FALSE, factor_key = FALSE) {
  key <- as_string(enexpr(key))
  new_key <- c(key_vars(data), key)
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
    ordered = is_ordered(data), interval = interval(data), validate = FALSE
  )
}

#' @inheritParams tidyr::spread
#' @rdname tsibble-tidyverse
#' @export
spread.tbl_ts <- function(data, key, value, ...) {
  key <- enexpr(key)
  value <- enexpr(value)
  key_var <- vars_pull(names(data), !! key)
  if (has_index(key_var, data)) {
    abort(paste_inline(
      sprintf("Column `%s` (index) can't be spread.", key_var),
      "Please use `as_tibble()` to coerce."
    ))
  }
  key_left <- setdiff(key_vars(data), key_var)
  new_key <- key_vars(remove_key(data, .vars = key_left))

  tbl <- spread(as_tibble(data), key = !! key, value = !! value, ...)
  tbl <- retain_tsibble(tbl, new_key, index(data))

  vars <- names(tbl)
  data <- mutate_index2(data, vars)
  build_tsibble(
    tbl, key = !! new_key, index = !! index(data), index2 = !! index2(data), 
    ordered = is_ordered(data), interval = is_regular(data), 
    validate = FALSE
  )
}

#' @inheritParams tidyr::nest
#' @rdname tsibble-tidyverse
#' @examples
#' nested_stock <- stocksm %>% 
#'   nest(-stock)
#' stocksm %>% 
#'   group_by(stock) %>% 
#'   nest()
#' @export
nest.tbl_ts <- function(.data, ...) {
  tbl_nest <- tidyr::nest(as_tibble(.data), ...)
  data_names <- names(.data)
  nest_names <- names(tbl_nest)
  nest_vars <- setdiff(data_names, nest_names)
  if (!has_all_key(nest_vars, .data) && !has_index(nest_vars, .data)) {
    build_tsibble(tbl_nest, key = setdiff(key_vars(.data), nest_vars),
      index = !! index(.data), validate = FALSE)
  } else if (!has_index(nest_vars, .data)) {
    build_tsibble(tbl_nest, index = !! index(.data), validate = FALSE)
  } else {
    new_lst <- nest_names[map_lgl(tbl_nest, is_list)]
    old_lst <- data_names[map_lgl(data_names, is_list)]
    lst_vars <- setdiff(new_lst, old_lst)
    .data <- select_tsibble(ungroup(.data), !!! nest_vars, validate = FALSE)
    tbl_nest[[lst_vars]] <- lapply(tbl_nest[[lst_vars]],
      function(x) update_meta(x, .data))
    as_lst_ts(tbl_nest)
  }
}

unnest.lst_ts <- function(data, ..., key = NULL) {
  if (utils::packageVersion("tidyr") > "0.8.3") {
    abort(paste_inline(
      "Can't unnest to a tsibble due to the API changes in `tidyr::unnest()`.",
      "Please use `unnest_tsibble()` instead."
    ))
  }
  inform(paste_inline(
    "`unnest()` to a tsibble is deprecated due to the forthcoming tidyr release.",
    "Please use `unnest_tsibble()` instead."
  ))
  unnested_data <- unnest(as_tibble(data), ...)

  key <- use_id(data, !! enquo(key))
  lst_cols <- setdiff(names(data), names(unnested_data))

  # checking if the nested columns has `tbl_ts` class (only for the first row)
  first_nested <- data[lst_cols][1L, ]
  eval_col <- purrr::imap(first_nested, dplyr::first)
  tsbl_col <- map_lgl(eval_col, is_tsibble)
  if (sum(tsbl_col) == 0) return(unnested_data)

  tsbl <- eval_col[tsbl_col][[1L]]
  idx <- index(tsbl)
  key <- c(key_vars(tsbl), key)
  unnested_data <- unnest_check_tsibble(unnested_data, key, idx)

  idx_chr <- as_string(idx)
  # restore the index class, as it's dropped by NextMethod()
  class(unnested_data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(
    unnested_data, key = !! key, index = !! idx, index2 = !! index2(tsbl),
    ordered = is_ordered(tsbl), interval = is_regular(tsbl), validate = FALSE
  )
}

unnest.tbl_ts <- function(data, ..., key = NULL) {
  if (utils::packageVersion("tidyr") > "0.8.3") {
    abort(paste_inline(
      "Can't unnest to a tsibble due to the API changes in `tidyr::unnest()`.",
      "Please use `unnest_tsibble()` instead."
    ))
  }
  inform(paste_inline(
    "`unnest()` to a tsibble is deprecated due to the forthcoming tidyr release.",
    "Please use `unnest_tsibble()` instead."
  ))
  unnested_data <- unnest(as_tibble(data), ...)

  key <- use_id(data, !! enquo(key))
  key <- c(key_vars(data), key)
  idx <- index(data)
  unnested_data <- unnest_check_tsibble(unnested_data, key, idx)

  idx_chr <- as_string(idx)
  class(unnested_data[[idx_chr]]) <- class(data[[idx_chr]])
  build_tsibble(
    unnested_data, key = !! key, index = !! idx, index2 = !! index2(data), 
    ordered = is_ordered(data), interval = is_regular(data), validate = FALSE
  )
}

# used for unnest() to check if the tsibble holds
unnest_check_tsibble <- function(data, key, index) {
  is_dup <- duplicated_key_index(data, key, index)
  if (is_dup) {
    header <- "The result is not a valid tsibble.\n"
    hint <- "Do you forget argument `key` in `unnest()` to create the key?."
    abort(paste0(header, hint))
  }
  data
}

#' @param data A tsibble or a data frame contains tsibble in the list-columns.
#' @param cols Names of columns to unnest.
#' @inheritParams as_tsibble
#' @rdname tsibble-tidyverse
#' @examples
#' nested_stock %>% 
#'   unnest_tsibble(cols = data, key = stock)
#' stock_qtl <- stocksm %>% 
#'   group_by(stock) %>% 
#'   index_by(day3 = lubridate::floor_date(time, unit = "3 day")) %>% 
#'   summarise(
#'     value = list(quantile(price)), 
#'     qtl = list(c("0%", "25%", "50%", "75%", "100%"))
#'   )
#' unnest_tsibble(stock_qtl, cols = c(value, qtl), key = c(stock, qtl))
#' @export
unnest_tsibble <- function(data, cols, key = NULL, validate = TRUE) {
  if (is_false(inherits(data, "lst_ts") || is_tsibble(data))) {
    abort("`data` contains no tsibble object.")
  }
  if (missing(cols)) {
    abort("Argument `cols` for columns to unnest is required.")
  }
  if (utils::packageVersion("tidyr") > "0.8.3") {
    unnested_data <- unnest(as_tibble(data), cols = !! enquo(cols))
  } else {
    cols_lst <- syms(vars_select(names(data), !! enquo(cols)))
    unnested_data <- unnest(as_tibble(data), !!! cols_lst)
  }

  if (is_tsibble(data)) {
    idx <- index(data)
    tsbl <- data
  } else {
    lst_cols <- setdiff(names(data), names(unnested_data))
    # checking if the nested columns has `tbl_ts` class (only for the first row)
    first_nested <- data[lst_cols][1L, ]
    eval_col <- purrr::imap(first_nested, dplyr::first)
    tsbl_col <- map_lgl(eval_col, is_tsibble)
    if (sum(tsbl_col) == 0) {
      abort("Unnested columns contain no tsibble object.")
    }

    tsbl <- eval_col[tsbl_col][[1L]]
    idx <- index(tsbl)
  }

  key <- use_id(unnested_data, !! enquo(key))
  idx_chr <- as_string(idx)
  class(unnested_data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(
    unnested_data, key = !! key, index = !! idx, index2 = !! index2(tsbl), 
    ordered = is_ordered(tsbl), interval = is_regular(tsbl), validate = validate
  )
}

fill.tbl_ts <- function(data, ..., .direction = c("down", "up")) {
  res <- NextMethod()
  update_meta2(res, data, ordered = is_ordered(data), interval = interval(data))
}

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

filter.lst_ts <- mutate.lst_ts

#' @export
slice.lst_ts <- mutate.lst_ts

#' @export
group_by.lst_ts <- mutate.lst_ts

#' @export
left_join.lst_ts <- function(x, ...) {
  as_lst_ts(NextMethod())
}

#' @export
right_join.lst_ts <- left_join.lst_ts

#' @export
full_join.lst_ts <- left_join.lst_ts

#' @export
inner_join.lst_ts <- left_join.lst_ts

#' @export
anti_join.lst_ts <- left_join.lst_ts

#' @export
semi_join.lst_ts <- left_join.lst_ts

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
