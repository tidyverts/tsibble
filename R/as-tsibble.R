globalVariables(c("key", "value", "zzz"))

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
#' @details A tsibble is sorted by its key(s) first and index.
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
  tbl <- tibble::tibble(...)
  index <- enquo(index)
  build_tsibble(tbl, key = key, index = !! index, regular = regular)
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
    x, key = key, index = !! index, regular = regular,
    validate = validate
  )
}

#' @rdname as-tsibble
#' @export
as_tsibble.tbl_ts <- function(
  x, key = id(), index, regular = TRUE, validate = TRUE, ...
) {
  if (is_empty(key)) {
    key <- key(x)
  }
  index <- enquo(index)
  if (quo_is_missing(index)) {
    index <- index(x)
  }
  build_tsibble(
    x, key = key, index = !! index, regular = regular,
    validate = validate
  )
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
    x, key = key, index = !! index, groups = groups, regular = regular,
    validate = validate
  )
}

#' @keywords internal
#' @export
as_tsibble.grouped_ts <- as_tsibble.grouped_df

#' @keywords internal
#' @export
as_tsibble.default <- function(x, ...) {
  cls <- class(x)[1]
  msg <- sprintf(
    "`as_tsibble()` doesn't know how to coerce the `%s` class yet.", cls
  )
  abort(msg)
}

#' @keywords internal
#' @export
as_tsibble.NULL <- function(x, ...) {
  abort("A tsibble must not be NULL.")
}

#' @export
groups.tbl_ts <- function(x) {
  attr(x, "vars")
}

#' @export
group_vars.tbl_ts <- function(x) {
  character(0L)
}

#' @export
group_vars.grouped_ts <- function(x) {
  key_flatten(groups(x))
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
  if (is_false(is_tsibble(x))) {
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
#' A relatively more flexible function to create a `tbl_ts` object. It is useful
#' for creating a `tbl_ts` internally inside a function, and it allows users to
#' determine if the time needs ordering and the interval needs calculating.
#'
#' @param x A `data.frame`, `tbl_df`, `tbl_ts`, or other tabular objects.
#' @param key Structural variable(s) that define unique time indices, used with
#' the helper [id]. If a univariate time series (without an explicit key),
#' simply call `id()`.
#' @inheritParams as_tsibble
#' @param groups Grouping variable(s).
#' @param ordered The default of `NULL` arranges the key variable(s) first and
#' then index in ascending order. `TRUE` suggests to skip the ordering as `x` in
#' the correct order. `FALSE` also skips the ordering but gives a warning instead.
#' @param interval `NULL` computes the interval. Use the specified interval as
#' is, if an class of `interval` is supplied.
#'
#' @export
build_tsibble <- function(
  x, key, index, groups = id(), regular = TRUE,
  validate = TRUE, ordered = NULL, interval = NULL
) {
  if (NROW(x) == 0 || has_length(x[[1]], 0)) { # no elements or length of 0
    abort("A tsibble must not be empty.")
  }
  # if key is quosures
  use_id(key)

  tbl <- ungroup(as_tibble(x, validate = validate)) # x is lst, data.frame, tbl_df
  # extract or pass the index var
  index <- extract_index_var(tbl, enquo(index))
  # (1) validate and process key vars (from expr to a list of syms)
  key_vars <- validate_key(data = tbl, key)
  # (2) if there exists a list of lists, flatten it as characters
  flat_keys <- key_flatten(key_vars)
  # (3) index cannot be part of the keys
  idx_chr <- quo_text2(index)
  is_index_in_keys <- intersect(idx_chr, flat_keys)
  if (is_false(is_empty(is_index_in_keys))) {
    abort(sprintf("`%s` can't be both `key` and `index`.", idx_chr))
  }
  # validate tbl_ts
  if (validate) {
    tbl <- validate_tbl_ts(data = tbl, key = key_vars, index = index)
    tbl <- validate_nested(data = tbl, key = key_vars)
  }
  if (is_false(regular)) {
    interval <- list()
  } else if (regular && is.null(interval)) {
    eval_idx <- eval_tidy(index, data = tbl)
    interval <- pull_interval(eval_idx, duplicated = TRUE)
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
      msg <- sprintf("The `tbl_ts` is not sorted by `%s`.", idx_chr)
    } else {
      msg <- sprintf(
        "The `tbl_ts` is not sorted by `%s`, and `%s`.",
        paste_comma(format(key)), idx_chr
      )
    }
    warn(msg)
  } # true do nothing

  # grped_key <- grouped_df(tbl, flat_keys)
  tbl <- tibble::new_tibble(
    tbl,
    "key" = structure(key_vars, class = "key"),
    # "key_indices" = attr(grped_key, "indices"),
    "index" = index,
    "interval" = structure(interval, class = "interval"),
    "regular" = regular,
    "ordered" = ordered,
    subclass = "tbl_ts"
  )

  if (is_empty(groups)) {
    return(tbl)
  }

  # convert grouped_df to tsibble:
  # the `groups` arg must be supplied, otherwise returns a `tbl_ts` not grouped
  groups <- validate_key(x, groups)
  tbl <- validate_nested(data = tbl, key = groups)

  flat_grps <- key_flatten(groups)
  grped_df <- grouped_df(tbl, flat_grps)
  tibble::new_tibble(
    grped_df,
    "vars" = structure(groups, class = "vars"),
    subclass = c("grouped_ts", "tbl_ts")
  )
}

detect_type <- function() {
  c("time", "dttm", "date", "week", "mth", "qtr")
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
  unname(enquos(...))
}

## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects.
extract_index_var <- function(data, index) {
  idx_type <- purrr::map_chr(data, index_sum)
  is_quo <- is_quosure(index)
  if (quo_is_null(index)) {
    abort("The `index` has been dropped somehow. Please reconstruct the `tbl_ts`.")
  }
  if (quo_is_missing(index)) {
    val_idx <- idx_type %in% detect_type()
    if (sum(val_idx) != 1) {
      abort("Can't determine the `index`. Please specify the `index` arg.")
    }
    chr_index <- colnames(data)[val_idx]
    inform(sprintf("The `index` is `%s`.", chr_index))
    return(sym(chr_index))
  } else {
    chr_index <- quo_text2(index)
    idx_na <- idx_type[chr_index]
    if (is.na(idx_na)) {
      cls_idx <- purrr::map_chr(data, ~ class(.)[1])
      name_idx <- names(idx_na)
      abort(sprintf(
        "Unsupported index type: `%s`", cls_idx[colnames(data) %in% name_idx])
      )
    }
    sym(chr_index)
  }
}

# check if the number of unique values is in descending order for a set of
# nested variables
validate_nested <- function(data, key) {
  nest_lgl <- is_nest(key)
  if (any(nest_lgl)) {
    key_nest <- key[nest_lgl]
    nest_keys <- purrr::map(
      key_nest, ~ purrr::map_chr(., quo_text2)
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
validate_tbl_ts <- function(data, key, index) {
  idx <- quo_text2(index)
  # NOTE: bug in anyDuplicated.data.frame()
  # identifiers <- c(key_flatten(key), idx)
  # below calls anyDuplicated.data.frame():
  # time zone associated with the index will be dropped,
  # e.g. nycflights13::weather, thus result in duplicates.
  # dup <- anyDuplicated(data[, identifiers, drop = FALSE])
  tbl_dup <- grouped_df(data, vars = key_flatten(key)) %>%
    summarise(zzz = anyDuplicated.default(!! index))
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
  drop_tsibble(x)
}

#' @export
as.tibble.tbl_ts <- as_tibble.tbl_ts

#' @export
as_tibble.grouped_ts <- function(x, ...) {
  dx <- drop_tsibble(x)
  tibble::new_tibble(
    dx,
    "vars" = structure(flatten(groups(x)), class = "vars"),
    subclass = "grouped_df"
  )
}

#' @export
as.tibble.grouped_ts <- as_tibble.grouped_ts

#' @rdname as-tibble
#' @export
as.data.frame.tbl_ts <- function(x, ...) {
  x <- drop_tsibble(x)
  NextMethod()
}

use_id <- function(x) {
  tryCatch(
    is_quosures(x),
    error = function(e) {
      e$call <- NULL
      e$message <- "Have you forgotten `tsibble::id()` to create the `key`?"
      stop(e)
    }
  )
}

drop_tsibble <- function(x) {
  attr(x, "key") <- attr(x, "index") <- NULL
  attr(x, "interval") <- attr(x, "regular") <- attr(x, "ordered") <- NULL
  tibble::new_tibble(x)
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
  use_id(key)
  index <- extract_index_var(data, enquo(index))

  grouped_df(data, vars = key_flatten(key)) %>%
    mutate(zzz = duplicated.default(!! index, fromLast = fromLast)) %>%
    dplyr::pull(zzz)

  # identifiers <- c(key_flatten(key), quo_text2(index))
  # duplicated(data[, identifiers, drop = FALSE], fromLast = fromLast)
  # not handling time zone correctly for duplicated.data.frame
}
