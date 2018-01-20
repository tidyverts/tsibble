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
#' finds the minimal time span as the interval.
#'
#' @inheritSection tsibble-package Index
#'
#' @inheritSection tsibble-package Key
#'
#' @inheritSection tsibble-package Interval
#'
#' @details A valid tsibble does not arrange the index in the time order.
#' Please use [arrange] to get the order by time.
#'
#' @return A tsibble object.
#'
#' @examples
#' # create a tsibble w/o a key ----
#' tbl1 <- tsibble(
#'   date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
#'   value = rnorm(10),
#'   key = id(), index = date
#' )
#' tbl1
#'
#' # create a tsibble with one key ----
#' tbl2 <- tsibble(
#'   qtr = rep(yearquarter(seq(2010, 2012.25, by = 1/ 4)), 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30),
#'   key = id(group), index = qtr
#' )
#' tbl2
#'
#' @export
tsibble <- function(..., key = id(), index, regular = TRUE) {
  tbl <- tibble::tibble(...)
  index <- enquo(index)
  as_tsibble(tbl, key = key, index = !! index, regular = regular)
}

#' Coerce to a tsibble object
#'
#' @param x Other objects to be coerced to a tsibble (`tbl_ts`).
#' @param key Structural variable(s) that define unique time indices, used with
#' the helper [id]. If a univariate time series (without an explicit key),
#' simply call `id()`.See below for details.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param regular Regular time interval (`TRUE`) or irregular (`FALSE`). `TRUE`
#' finds the minimal time span as the interval.
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
  tsibble_tbl(
    x, key = key, index = index, regular = regular,
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
  # convert grouped_df to tsibble:
  # the `groups` arg must be supplied, otherwise returns a `tbl_ts` not grouped
  index <- enquo(index)

  tbl <- tsibble_tbl(
    x, key = key, index = index, regular = regular,
    validate = validate
  )

  if (is_empty(groups)) {
    return(tbl)
  }

  groups <- validate_key(x, groups)
  tbl <- validate_nested(data = tbl, key = groups)

  flat_grps <- flatten_key(groups)
  grped_df <- grouped_df(tbl, flat_grps)
  tibble::new_tibble(
    grped_df,
    "vars" = structure(groups, class = "vars"),
    subclass = c("grouped_ts", "tbl_ts")
  )
}

#' @keywords internal
#' @export
as_tsibble.grouped_ts <- as_tsibble.grouped_df

#' @keywords internal
#' @export
as_tsibble.default <- function(x, ...) {
  abort("as_tsibble doesn't know how to deal with this type of class yet.")
}

#' @keywords internal
#' @export
as_tsibble.NULL <- function(x, ...) {
  abort("A tsibble cannot be empty or NULL.")
}

#' Return key and measured variables
#'
#' `key()` returns a list of symbols; `key_vars()` gives a character vector.
#'
#' @param x A data frame.
#'
#' @rdname key
#'
#' @examples
#' # A single key for pedestrian data ----
#' data(pedestrian)
#' key(pedestrian)
#' key_vars(pedestrian)
#' measured_vars(pedestrian)
#'
#' # Nested and crossed keys for tourism data ----
#' data(tourism)
#' key(tourism)
#' key_vars(tourism)
#' measured_vars(tourism)
#' @export
key <- function(x) {
  UseMethod("key")
}

#' @export
key.tbl_ts <- function(x) {
  attr(x, "key")
}

`key<-` <- function(x, value) {
  attr(x, "key") <- value
  x
}

#' @rdname key
#' @export
key_vars <- function(x) {
  UseMethod("key_vars")
}

#' @export
key_vars.tbl_ts <- function(x) {
  format(key(x))
}

#' Compute sizes of key variables
#'
#' @param x A data frame.
#'
#' @examples
#' key_size(pedestrian)
#' n_keys(pedestrian)
#'
#' @rdname key-size
#' @export
key_size <- function(x) {
  UseMethod("key_size")
}

#' @export
key_size.tbl_ts <- function(x) {
  vapply(attr(x, "key_indices"), length, integer(1))
}

#' @rdname key-size
#' @export
n_keys <- function(x) {
  UseMethod("n_keys")
}

#' @export
n_keys.tbl_ts <- function(x) {
  length(key_size(x))
}

#' @export
groups.tbl_ts <- function(x) {
  attr(x, "vars")
}

#' @export
group_vars.tbl_ts <- function(x) {
  format(groups(x))
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

#' @rdname key
#' @export
measured_vars <- function(x) {
  UseMethod("measured_vars")
}

#' @rdname key
#' @export
measured_vars.tbl_ts <- function(x) {
  all_vars <- dplyr::tbl_vars(x)
  key_vars <- flatten_key(key(x))
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
  if (is_false(is_tsibble(x))) {
    abort(paste(expr_label(substitute(x)), "is not a tsibble."))
  }
  attr(x, "interval")
}

#' @rdname index-rd
#' @export
index <- function(x) {
  if (is_false(is_tsibble(x))) {
    abort(paste(expr_label(substitute(x)), "is not a tsibble."))
  }
  attr(x, "index")
}

`index<-` <- function(x, value) {
  attr(x, "index") <- value
  x
}

#' If a tsibble is spaced at regular time or not
#'
#' @param x A tsibble object.
#' @rdname regular
#' @aliases is.regular
#' @examples
#' data(pedestrian)
#' is_regular(pedestrian)
#' @export
is_regular <- function(x) {
  if (is_false(is_tsibble(x))) {
    abort(paste(expr_label(substitute(x)), "is not a tsibble."))
  }
  attr(x, "regular")
}

#' @rdname regular
#' @export
is.regular <- is_regular

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
tsibble_tbl <- function(x, key, index, regular = TRUE, validate = TRUE) {
  if (NROW(x) == 0) {
    abort("A tsibble cannot be empty or NULL.")
  }
  # if key is quosures
  use_id(key)

  tbl <- ungroup(as_tibble(x)) # x is lst, data.frame, tbl_df
  # extract or pass the index var
  index <- extract_index_var(tbl, index = index)
  # (1) validate and process key vars (from expr to a list of syms)
  key_vars <- validate_key(data = tbl, key)
  # (2) if there exists a list of lists, flatten it as characters
  flat_keys <- flatten_key(key_vars)
  # (3) index cannot be part of the keys
  is_index_in_keys <- intersect(quo_text2(index), flat_keys)
  if (is_false(is_empty(is_index_in_keys))) {
    abort("The index variable cannot be one of the keys.")
  }
  # validate tbl_ts
  if (validate) {
    tbl <- validate_tbl_ts(data = tbl, key = key_vars, index = index)
    tbl <- validate_nested(data = tbl, key = key_vars)
  }
  tbl_interval <- list()
  if (regular) {
    eval_idx <- eval_tidy(index, data = tbl)
    tbl_interval <- pull_interval(eval_idx, duplicated = TRUE)
  }

  grped_key <- grouped_df(tbl, flat_keys)
  tibble::new_tibble(
    tbl,
    "key" = structure(key_vars, class = "key"),
    "key_indices" = attr(grped_key, "indices"),
    "index" = index,
    "interval" = structure(tbl_interval, class = "interval"),
    "regular" = regular,
    subclass = "tbl_ts"
  )
}

detect_type <- function() {
  c("time", "dttm", "date", "mth", "qtr")
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
  quos(...)
}

## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects.
extract_index_var <- function(data, index) {
  idx_type <- purrr::map_chr(data, index_sum)
  if (quo_is_missing(index)) {
    val_idx <- idx_type %in% detect_type()
    if (sum(val_idx) != 1) {
      abort("Please specify the 'index' argument.")
    }
    chr_index <- colnames(data)[val_idx]
    inform(paste("The 'index' variable:", chr_index))
    idx_sym <- sym(chr_index)
    index <- as_quosure(idx_sym)
    return(index)
  } else {
    idx_na <- idx_type[quo_text2(index)]
    if (is.na(idx_na)) {
      cls_idx <- purrr::map_chr(data, ~ class(.)[1])
      abort(paste(
        "Unsupported index type:",
        cls_idx[colnames(data) %in% names(idx_na)])
      )
    }
  }
  index
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
      msg <- purrr::map(which_bad, ~ paste(., collapse = " | "))
      msg <- paste_comma(msg)
      abort(paste0(
        "Incorrect ordering of nested variables: ",
        msg
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
  # identifiers <- c(flatten_key(key), idx)
  # below calls anyDuplicated.data.frame():
  # time zone associated with the index will be dropped, 
  # e.g. nycflights13::weather, thus result in duplicates. 
  # dup <- anyDuplicated(data[, identifiers, drop = FALSE])
  tbl_dup <- grouped_df(data, vars = flatten_key(key)) %>%
    summarise(zzz = anyDuplicated(!! index))
  if (any_not_equal_to_c(tbl_dup$zzz, 0)) {
    msg <- paste("Invalid tsibble: identical data entries from", idx)
    if (!is_empty(key)) {
      class(key) <- "key"
      msg <- paste0(paste(msg, "and", paste_comma(format(key))), ".")
    }
    msg <- paste(msg, "Use `inform_duplidates()` to check the duplicated rows.")
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
  class(x) <- "data.frame"
  x
}

use_id <- function(x) {
  tryCatch(
    is_quosures(x),
    error = function(e) stop("Please use `tsibble::id()` to create the 'key'.")
  )
}

drop_tsibble <- function(x) {
  attr(x, "key") <- attr(x, "key_indices") <- attr(x, "index") <- NULL
  attr(x, "interval") <- attr(x, "regular") <- NULL
  tibble::new_tibble(x)
}

#' Inform duplication of key and index variables
#'
#' Inform which row has duplicated key and index elements
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
inform_duplidates <- function(data, key = id(), index, fromLast = FALSE) {
  use_id(key)
  index <- extract_index_var(data, enquo(index))

  identifiers <- c(flatten_key(key), quo_text2(index))
  duplicated(data[, identifiers, drop = FALSE], fromLast = fromLast)
}
