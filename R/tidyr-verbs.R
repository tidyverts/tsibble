#' Gather columns into key-value pairs.
#'
#' @param data A `tbl_ts`.
#' @inheritParams tidyr::gather
#'
#' @seealso [tidyr::gather]
#' @rdname gather
#' @export
#' @examples
#' # tidyr example
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
  tbl <- gather(
    as_tibble(data), key = !! key, value = !! value, ...,
    na.rm = na.rm, convert = convert, factor_key = factor_key
  )
  build_tsibble(
    tbl, key = new_key, index = !! index(data), index2 = !! index2(data),
    groups = groups(data), regular = is_regular(data), validate = FALSE,
    ordered = is_ordered(data), interval = interval(data)
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
#' # tidyr example
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
  build_tsibble(
    tbl, key = new_key, index = !! index(data), index2 = !! index2(data),
    regular = is_regular(data), validate = FALSE, ordered = is_ordered(data),
    interval = interval(data)
  )
}

# library(rlang)
# lst_ped <- pedestrian %>% 
#   # group_by(Sensor) %>% 
#   tidyr::nest(-Sensor)
# lst_ped %>% 
#   tidyr::unnest()
# lst_ped2 <- pedestrian %>% 
#   index_by(yrmth = yearmonth(Date)) %>% 
#   group_by(Sensor) %>% 
#   tidyr::nest()
# lst_ped2 %>% 
#   tidyr::unnest()
# pedestrian %>% 
#   group_by(Sensor) %>% 
#   tidyr::nest()
# tidyr::unnest(lst_ped)
# lst_t <- tourism %>% 
#   group_by(Purpose) %>% 
#   tidyr::nest()
#
# lst_t %>% 
#   tidyr::unnest()

# #' @export
# nest.tbl_ts <- function(data, ..., .key = "data") {
#   nest_quos <- enquos(...)
#   key_var <- quo_name(enexpr(.key))
#   cn <- names(data)
#   if (is_empty(nest_quos)) {
#     nest_vars <- cn
#   } else {
#     nest_vars <- tidyselect::vars_select(cn, !!! nest_quos)
#   }
#   if (is_false(has_index(nest_vars, data))) {
#     abort("`nest.tbl_ts()` must have the `index` in the list of data columns.")
#   }
#   tbl <- as_tibble(data)
#   if (dplyr::is_grouped_df(tbl)) {
#     grp_vars <- group_vars(tbl)
#   } else {
#     grp_vars <- setdiff(cn, nest_vars)
#   }
#   data <- ungroup(data)
#   if (is_empty(grp_vars)) {
#     return(tibble::tibble(!! key_var := list(data)))
#   }
#   nest_vars <- setdiff(nest_vars, grp_vars)
#   grp <- syms(grp_vars)
#   nest_df <- split_by(data, !!! grp)
#   out <- distinct(data, !!! grp)
#   out[[key_var]] <- purrr::map(
#     nest_df, ~ tsibble_select(., !!! nest_vars, validate = FALSE)
#   )
#   idx <- index(data)
#   tibble::new_tibble(
#     out, 
#     key = key(data), 
#     index = idx, 
#     regular = is_regular(data),
#     # dark: work around for unnest(), since it drops the index class
#     index_class = class(eval_tidy(idx, data)), 
#     subclass = "lst_ts"
#   )
# }
#
# #' @export
# unnest.lst_ts <- function(data, ..., 
#   .drop = NA, .id = NULL, .sep = NULL, .preserve = NULL
# ) {
#   out <- NextMethod()
#   idx <- index(data)
#   idx_chr <- quo_text(idx)
#   # restore the index class, as it's dropped by NextMethod()
#   class(out[[idx_chr]]) <- attr(data, "index_class")
#   build_tsibble(
#     out, key = key(data), index = !! idx, validate = FALSE, 
#     regular = is_regular(data),
#   )
# }
