#' Create a tsibble object
#'
#' @param ... A set of name-value pairs. The names of "key" and "index" should
#' be avoided as they are used as the arguments.
#' @param key Structural variable(s) that define unique time indices, used with
#' the helper [id]. If a univariate time series (without an explicit key),
#' simply call `id()`. See below for details.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param regular Regular time interval (`TRUE`) or irregular (`FALSE`). `TRUE`
#' finds the greatest common divisor of positive time distances as the interval.
#'
#' @inheritSection tsibble-package Index
#'
#' @inheritSection tsibble-package Key
#'
#' @inheritSection tsibble-package Interval
#'
#' @details A tsibble is sorted by its key first and index.
#'
#' @return A tsibble object.
#' @seealso [build_tsibble]
#'
#' @examples
#' # create a tsibble w/o a key ----
#' tsbl1 <- tsibble(
#'   date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
#'   value = rnorm(10),
#'   key = id(), index = date
#' )
#' tsbl1
#'
#' # create a tsibble with one key ----
#' tsbl2 <- tsibble(
#'   qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30),
#'   key = id(group), index = qtr
#' )
#' tsbl2
#'
#' @export
tsibble <- function(..., key = id(), index, regular = TRUE) {
  dots <- rlang::list2(...)
  if (is_empty(dots)) {
    abort("A tsibble must not be empty.")
  }
  if (has_length(dots, 1) && is.data.frame(dots[[1]])) {
    abort("Must not be a data frame, do you want `as_tsibble()`?")
  }
  tbl <- tibble::tibble(!!! dots)
  index <- enquo(index)
  build_tsibble(tbl, key = !! enquo(key), index = !! index, regular = regular)
}

#' Coerce to a tsibble object
#'
#' @param x Other objects to be coerced to a tsibble (`tbl_ts`).
#' @inheritParams tsibble
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables lead to unique time indices (i.e. a valid tsibble). It will
#' also make sure that the nested variables are arranged from lower level to
#' higher, if nested variables are passed to `key`. If you are sure that it's a
#' valid input, specify `FALSE` to skip the checks.
#' @param ... Other arguments passed on to individual methods.
#'
#' @return A tsibble object.
#' @rdname as-tsibble
#' @aliases as.tsibble
#' @seealso tsibble
#'
#' @examples
#' # coerce tibble to tsibble w/o a key ----
#' tbl1 <- tibble::tibble(
#'   date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
#'   value = rnorm(10)
#' )
#' as_tsibble(tbl1)
#' # specify the index var
#' as_tsibble(tbl1, index = date)
#'
#' # coerce tibble to tsibble with one key ----
#' # "date" is automatically considered as the index var, and "group" is the key
#' tbl2 <- tibble::tibble(
#'   mth = rep(yearmonth(seq(2017, 2017 + 9 / 12, by = 1 / 12)), 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30)
#' )
#' as_tsibble(tbl2, key = id(group))
#' as_tsibble(tbl2, key = id(group), index = mth)
#'
#' @export
as_tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

#' @rdname as-tsibble
#' @export
as_tsibble.tbl_df <- function(
  x, key = id(), index, regular = TRUE, validate = TRUE, ...
) {
  index <- enquo(index)
  build_tsibble(
    x, key = !! enquo(key), index = !! index, regular = regular,
    validate = validate
  )
}

#' @rdname as-tsibble
#' @export
as_tsibble.tbl_ts <- function(x, ...) {
  x
}

#' @rdname as-tsibble
#' @export
as_tsibble.data.frame <- as_tsibble.tbl_df

#' @keywords internal
#' @export
as_tsibble.tbl <- as_tsibble.tbl_df

#' @rdname as-tsibble
#' @export
as_tsibble.list <- as_tsibble.tbl_df

#' @keywords internal
#' @export
as_tsibble.grouped_df <- function(
  x, key = id(), index, groups = id(), regular = TRUE, validate = TRUE, ...
) {
  index <- enquo(index)
  build_tsibble(
    x, key = !! enquo(key), index = !! index, groups = !! enquo(groups), 
    regular = regular, validate = validate
  )
}

#' @keywords internal
#' @export
as_tsibble.grouped_ts <- as_tsibble.tbl_ts

#' @keywords internal
#' @export
as_tsibble.default <- function(x, ...) {
  dont_know(x, "as_tsibble")
}

#' @keywords internal
#' @export
as_tsibble.NULL <- function(x, ...) {
  abort("A tsibble must not be NULL.")
}

#' @export
groups.tbl_ts <- function(x) {
  NULL
}

#' @export
groups.grouped_ts <- function(x) {
  syms(group_vars(x))
}

#' @export
group_vars.tbl_ts <- function(x) {
  character(0L)
}

#' @export
group_vars.grouped_ts <- function(x) {
  attr(x, "vars")
}

#' @export
group_size.grouped_ts <- function(x) {
  vapply(attr(x, "indices"), length, integer(1))
}

#' @export
n_groups.tbl_ts <- function(x) {
  length(group_size(x))
}

#' @export
group_indices.grouped_ts <- function(.data, ...) {
  attr(.data, "indices")
}

#' Return measured variables
#'
#' @param x A `tbl_ts`.
#'
#' @rdname measured-vars
#' @examples
#' measured_vars(pedestrian)
#' measured_vars(tourism)
#' @export
measured_vars <- function(x) {
  UseMethod("measured_vars")
}

#' @export
measured_vars.tbl_ts <- function(x) {
  all_vars <- dplyr::tbl_vars(x)
  key_vars <- key_vars(x)
  idx_var <- quo_text(index(x))
  setdiff(all_vars, c(key_vars, idx_var))
}

#' Return index and interval from a tsibble
#'
#' @param x A tsibble object.
#' @rdname index-rd
#' @examples
#' data(pedestrian)
#' index(pedestrian)
#' interval(pedestrian)
#' @export
interval <- function(x) {
  not_tsibble(x)
  attr(x, "interval")
}

#' @rdname index-rd
#' @export
index <- function(x) {
  not_tsibble(x)
  attr(x, "index")
}

#' @rdname index-rd
#' @export
index2 <- function(x) {
  not_tsibble(x)
  attr(x, "index2")
}

#' `is_regular` checks if a tsibble is spaced at regular time or not; `is_ordered`
#' checks if a tsibble is ordered by key and index.
#'
#' @param x A tsibble object.
#' @rdname regular
#' @aliases is.regular
#' @examples
#' data(pedestrian)
#' is_regular(pedestrian)
#' is_ordered(pedestrian)
#' @export
is_regular <- function(x) {
  not_tsibble(x)
  attr(x, "regular")
}

#' @rdname regular
#' @export
is.regular <- is_regular

#' @rdname regular
#' @export
is_ordered <- function(x) {
  not_tsibble(x)
  attr(x, "ordered")
}

not_tsibble <- function(x) {
  if (is_false(is_tsibble(x) || inherits(x, "lst_ts"))) {
    abort(sprintf("%s is not a tsibble.", deparse(substitute(x))))
  }
}

#' Test if the object is a tsibble
#'
#' @param x An object.
#'
#' @return TRUE if the object inherits from the tbl_ts class.
#' @rdname is-tsibble
#' @aliases is.tsibble
#' @examples
#' # A tibble is not a tsibble ----
#' tbl <- tibble::tibble(
#'   date = seq(as.Date("2017-10-01"), as.Date("2017-10-31"), by = 1),
#'   value = rnorm(31)
#' )
#' is_tsibble(tbl)
#'
#' # A tsibble ----
#' tsbl <- as_tsibble(tbl, index = date)
#' is_tsibble(tsbl)
#' @export
is_tsibble <- function(x) {
  inherits(x, "tbl_ts")
}

#' @rdname is-tsibble
#' @usage NULL
#' @export
is.tsibble <- is_tsibble

#' @rdname is-tsibble
#' @export
is_grouped_ts <- function(x) {
  inherits(x, "grouped_ts")
}

#' @rdname is-tsibble
#' @usage NULL
#' @export
is.grouped_ts <- is_grouped_ts

#' @rdname as-tsibble
#' @export
#' @usage NULL
as.tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

## tsibble is a special class of tibble that handles temporal data. It
## requires a sequence of time index to be unique across every identifier.

#' Construct a tsibble object
#'
#' A relatively more controllable function to create a `tbl_ts` object. It is useful
#' for creating a `tbl_ts` internally inside a function, and it allows users to
#' determine if the time needs ordering and the interval needs calculating.
#'
#' @param x A `data.frame`, `tbl_df`, `tbl_ts`, or other tabular objects.
#' @inheritParams as_tsibble
#' @param index2 A candidate of `index` to update the index to a new one when
#' [index_by]. By default, it's identical to `index`.
#' @param groups Grouping variable(s) when [group_by.tbl_ts].
#' @param ordered The default of `NULL` arranges the key variable(s) first and
#' then index in ascending order. `TRUE` suggests to skip the ordering as `x` in
#' the correct order. `FALSE` also skips the ordering but gives a warning instead.
#' @param interval `NULL` computes the interval. Use the specified interval as
#' is, if an class of `interval` is supplied.
#'
#' @export
#' @examples
#' # Prepare `pedestrian` to use a new index `Date` ----
#' pedestrian %>%
#'   build_tsibble(
#'     key = key(.), index = !! index(.), index2 = Date, interval = interval(.)
#'   )
build_tsibble <- function(
  x, key, index, index2, groups = id(), regular = TRUE,
  validate = TRUE, ordered = NULL, interval = NULL
) {
  if (NROW(x) == 0 || has_length(x[[1]], 0)) { # no elements or length of 0
    abort("A tsibble must not be empty.")
  }
  # if key is quosures
  key <- use_id(x, !! enquo(key))

  # x is lst, data.frame, tbl_df, use ungroup()
  # x is tbl_ts, use as_tibble(ungroup = TRUE)
  tbl <- ungroup(as_tibble(x, validate = validate))
  # extract or pass the index var
  index <- validate_index(tbl, enquo(index))
  # if index2 not specified
  index2 <- enquo(index2)
  if (quo_is_missing(index2)) {
    index2 <- index
  } else {
    index2 <- validate_index(tbl, index2)
  }
  # (1) validate and process key vars (from expr to a list of syms)
  key_vars <- validate_key(tbl, key)
  # (2) if there exists a list of lists, flatten it as characters
  flat_keys <- key_flatten(key_vars)
  # (3) index cannot be part of the keys
  idx_chr <- c(quo_text(index), quo_text(index2))
  is_index_in_keys <- intersect(idx_chr, flat_keys)
  if (is_false(is_empty(is_index_in_keys))) {
    abort(sprintf("`%s` can't be `index`, as it's used as `key`.", idx_chr))
  }
  # validate tbl_ts
  if (validate) {
    tbl <- validate_tsibble(data = tbl, key = key_vars, index = index)
    tbl <- validate_nested(data = tbl, key = key_vars)
  }
  if (is_false(regular)) {
    interval <- list()
  } else if (regular && is.null(interval)) {
    eval_idx <- eval_tidy(index, data = tbl)
    interval <- pull_interval(eval_idx)
  } else if (is_false(inherits(interval, "interval"))) {
    int_cls <- class(interval)[1]
    abort(sprintf("`interval` must be the `interval` class not %s.", int_cls))
  }

  # arrange in time order (by key and index)
  if (is.null(ordered)) { # first time to create a tsibble
    tbl <- tbl %>%
      arrange(!!! syms(flat_keys), !! index)
    ordered <- TRUE
  } else if (is_false(ordered)) { # false returns a warning
    if (is_empty(key_vars)) {
      msg <- sprintf("The `tbl_ts` is not sorted by `%s`.", idx_chr[1])
    } else {
      msg <- sprintf(
        "The `tbl_ts` is not sorted by `%s`, and `%s`.",
        paste_comma(format(key)), idx_chr[1]
      )
    }
    warn(msg)
  } # true do nothing

  idx_lgl <- identical(index, index2)
  if (is_empty(groups) && idx_lgl) {
    return(tibble::new_tibble(
      tbl,
      "key" = structure(key_vars, class = "key"),
      # "key_indices" = attr(grped_key, "indices"),
      "index" = index,
      "index2" = index2,
      "interval" = structure(interval, class = "interval"),
      "regular" = regular,
      "ordered" = ordered,
      subclass = c("tbl_ts")
    ))
  }

  # convert grouped_df to tsibble:
  # the `groups` arg must be supplied, otherwise returns a `tbl_ts` not grouped
  if (idx_lgl) {
    grped_df <- tbl %>% group_by(!!! groups)
  } else {
    grped_df <- tbl %>% group_by(!!! groups, !! index2)
  }
  tibble::new_tibble(
    grped_df,
    "key" = structure(key_vars, class = "key"),
    # "key_indices" = attr(grped_key, "indices"),
    "index" = index,
    "index2" = index2,
    "interval" = structure(interval, class = "interval"),
    "regular" = regular,
    "ordered" = ordered,
    subclass = c("grouped_ts", "tbl_ts")
  )
}


#' Identifier to construct structural variables
#'
#' Impose a structure to a tsibble
#'
#' @param ... Variables passed to tsibble()/as_tsibble().
#'
#' @seealso [tsibble], [as_tsibble]
#' @export
id <- function(...) {
  enexprs(...)
}

## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects.
validate_index <- function(data, index) {
  val_idx <- purrr::map_lgl(data, index_valid)
  if (quo_is_null(index)) {
    abort("`index` must not be NULL.")
  }
  if (quo_is_missing(index)) {
    if (sum(val_idx, na.rm = TRUE) != 1) {
      abort("Can't determine the `index` and please specify.")
    }
    chr_index <- names(data)[val_idx]
    chr_index <- chr_index[!is.na(chr_index)]
    inform(sprintf("The `index` is `%s`.", chr_index))
  } else {
    chr_index <- tidyselect::vars_pull(names(data), !! index)
    idx_pos <- names(data) %in% chr_index
    val_lgl <- val_idx[idx_pos]
    if (is.na(val_lgl)) {
      return(sym(chr_index))
    } else if (!val_idx[idx_pos]) {
      cls_idx <- purrr::map_chr(data, ~ class(.)[1])
      abort(sprintf(
        "Unsupported index type: `%s`", cls_idx[idx_pos])
      )
    }
  }
  if (anyNA(data[[chr_index]])) {
    abort(sprintf("Column `%s` passed as `index` must not contain `NA`.", chr_index))
  }
  sym(chr_index)
}

# check if the number of unique values is in descending order for a set of
# nested variables
validate_nested <- function(data, key) {
  nest_lgl <- is_nest(key)
  if (any(nest_lgl)) {
    key_nest <- key[nest_lgl]
    nest_keys <- purrr::map(
      key_nest, ~ purrr::map_chr(., quo_text)
    )
    n_dist <- purrr::map(
      nest_keys, ~ purrr::map_int(., ~ dplyr::n_distinct(data[[.]]))
    )
    n_lgl <- purrr::map_lgl(n_dist, is_descending)
    if (is_false(all(n_lgl))) {
      which_bad <- key_nest[!n_lgl]
      wrong_nested <- purrr::map(which_bad,
        ~ paste(surround(., "`"), collapse = " | ")
      )
      wrong_nested <- paste_comma(wrong_nested)
      wrong_dim <- purrr::map_chr(n_dist, ~ paste(., collapse = " | "))
      abort(sprintf(
        "Incorrect nesting: %s (%s). Please see `?tsibble`.",
        wrong_nested, wrong_dim
      ))
    }
  }
  data
}

# check if a comb of key vars result in a unique data entry
# if TRUE return the data, otherwise raise an error
validate_tsibble <- function(data, key, index) {
  idx <- quo_text(index)
  # NOTE: bug in anyDuplicated.data.frame() (fixed in R 3.5.0)
  # identifiers <- c(key_flatten(key), idx)
  # below calls anyDuplicated.data.frame():
  # time zone associated with the index will be dropped,
  # e.g. nycflights13::weather, thus result in duplicates.
  # dup <- anyDuplicated(data[, identifiers, drop = FALSE])
  tbl_dup <- grouped_df(data, vars = key_flatten(key)) %>%
    summarise(!! "zzz" := anyDuplicated.default(!! index))
  if (any_not_equal_to_c(tbl_dup$zzz, 0)) {
    msg <- sprintf("Invalid tsibble: identical data entries from `%s`", idx)
    if (!is_empty(key)) {
      class(key) <- "key"
      msg <- sprintf("%s and `%s`.", msg, paste_comma(format(key)))
    } else {
      msg <- paste0(msg, ".")
    }
    msg <- paste(msg, "Use `find_duplicates()` to check the duplicated rows.")
    abort(msg)
  }
  data
}

#' Coerce to a tibble or data frame
#'
#' @param x A `tbl_ts`.
#' @param ... Ignored.
#'
#' @rdname as-tibble
#' @export
#' @examples
#' as_tibble(pedestrian)
#'
#' # a grouped tbl_ts -----
#' grped_ped <- pedestrian %>% group_by(Sensor)
#' as_tibble(grped_ped)
as_tibble.tbl_ts <- function(x, ...) {
  attr(x, "key") <- attr(x, "index") <- attr(x, "index2") <- NULL
  attr(x, "interval") <- attr(x, "regular") <- attr(x, "ordered") <- NULL
  tibble::new_tibble(x)
}

#' @export
as_tibble.grouped_ts <- function(x, ...) {
  group_by(NextMethod(), !!! groups(x))
}

#' @keywords internal
#' @export
as_tibble.lst_ts <- function(x, ...) {
  tibble::new_tibble(x)
}

#' @export
as.tibble.tbl_ts <- as_tibble.tbl_ts

#' @export
as.tibble.grouped_ts <- as_tibble.grouped_ts

#' @rdname as-tibble
#' @export
as.data.frame.tbl_ts <- function(x, ...) {
  x <- as_tibble(x)
  NextMethod()
}

use_id <- function(x, key) {
  key <- enquo(key)
  safe_key <- purrr::safely(eval_tidy)(
    get_expr(key), 
    env = child_env(get_env(key), id = id)
  )
  if (is_null(safe_key$error)) {
    fn <- function(x) {
      if (is_list(x)) all(purrr::map_lgl(x, fn)) else is_expression(x)
    }
    lgl <- fn(safe_key$result)
    if (lgl) {
      return(safe_key$result)
    }
  }
  abort("Have you forgotten `id()` to create the `key`?")
}

#' Find duplication of key and index variables
#'
#' Find which row has duplicated key and index elements
#'
#' @param data A `tbl_ts` object.
#' @param key Structural variable(s) that define unique time indices, used with
#' the helper [id]. If a univariate time series (without an explicit key),
#' simply call `id()`.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param fromLast `TRUE` does the duplication check from the last of identical
#' elements.
#'
#' @return A logical vector of the same length as the row number of `data`
#' @export
find_duplicates <- function(data, key = id(), index, fromLast = FALSE) {
  key <- use_id(data, !! enquo(key))
  index <- validate_index(data, enquo(index))

  grouped_df(data, vars = key_flatten(key)) %>%
    mutate(!! "zzz" := duplicated.default(!! index, fromLast = fromLast)) %>%
    dplyr::pull(!! "zzz")

  # identifiers <- c(key_flatten(key), quo_text(index))
  # duplicated(data[, identifiers, drop = FALSE], fromLast = fromLast)
  # not handling time zone correctly for duplicated.data.frame
}
