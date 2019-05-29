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
    abort(sprintf(
      "Column `%s` (index) can't be spread.\nPlease use `as_tibble()` to coerce.", 
      key_var
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
#' if (packageVersion("tidyr") <= "0.8.3") {
#'   stop("Please update the tidyr package to run these following examples.")
#' }
#' nested_stock <- stocksm %>% 
#'   nest(data = -stock)
#' stocksm %>% 
#'   group_by(stock) %>% 
#'   nest()
#' @export
nest.tbl_ts <- function(.data, ...) {
  data_cp <- ungroup(.data)
  data <- as_tibble(.data)
  tbl_nest <- NextMethod()
  nest_vars <- setdiff(names(data), names(tbl_nest))
  if (!has_all_key(nest_vars, data_cp) && !has_index(nest_vars, data_cp)) {
    update_meta(tbl_nest, data_cp)
  } else if (!has_index(nest_vars, data_cp)) {
    tbl_nest
  } else {
    lst_vars <- setdiff(names(tbl_nest), names(data))
    data_cp <- select_tsibble(data_cp, !!! nest_vars, validate = FALSE)
    tbl_nest[[lst_vars]] <- lapply(tbl_nest[[lst_vars]],
      function(x) update_meta(x, data_cp))
    as_lst_ts(tbl_nest)
  }
}

unnest.lst_ts <- function(data, ..., key = NULL) {
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
  unnested_data <- unnest_tsibble(unnested_data, key, idx)

  idx_chr <- as_string(idx)
  # restore the index class, as it's dropped by NextMethod()
  class(unnested_data[[idx_chr]]) <- class(tsbl[[idx_chr]])
  build_tsibble(
    unnested_data, key = !! key, index = !! idx, index2 = !! index2(tsbl),
    ordered = is_ordered(tsbl), interval = is_regular(tsbl), validate = FALSE
  )
}

#' @param key Unquoted variables to create the key after unnesting.
#' @inheritParams tidyr::unnest
#' @rdname tsibble-tidyverse
#' @examples
#' nested_stock %>% 
#'   unnest(key = stock)
#' stock_qtl <- stocksm %>% 
#'   group_by(stock) %>% 
#'   index_by(day3 = lubridate::floor_date(time, unit = "3 day")) %>% 
#'   summarise(
#'     value = list(quantile(price)), 
#'     qtl = list(c("0%", "25%", "50%", "75%", "100%"))
#'   )
#' unnest(stock_qtl, key = qtl)
#' @export
unnest.tbl_ts <- function(data, ..., key = NULL) {
  unnested_data <- unnest(as_tibble(data), ...)

  key <- use_id(data, !! enquo(key))
  key <- c(key_vars(data), key)
  idx <- index(data)
  unnested_data <- unnest_tsibble(unnested_data, key, idx)

  idx_chr <- as_string(idx)
  class(unnested_data[[idx_chr]]) <- class(data[[idx_chr]])
  build_tsibble(
    unnested_data, key = !! key, index = !! idx, index2 = !! index2(data), 
    ordered = is_ordered(data), interval = is_regular(data), validate = FALSE
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
