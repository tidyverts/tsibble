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
#' @details A tsibble is arranged by the key and index in the time order.
#'
#' @return A tsibble object.
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
key.default <- function(x) {
  abort(sprintf("Can't find the `key` in `%s`", class(x)[1]))
}

#' @export
key.tbl_ts <- function(x) {
  attr(x, "key")
}

#' @rdname key
#' @export
unkey <- function(x) {
  UseMethod("unkey")
}

#' @rdname key
#' @export
unkey.tbl_ts <- function(x) {
  nkey <- n_keys(x)
  if (nkey < 2 || nkey == nrow(x)) {
    attr(x, "key") <- structure(id(), class = "key")
    return(x)
  } else {
    abort("`unkey()` must not be applied to a `tbl_ts` of more than 1 key size.")
  }
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
#' key_indices(pedestrian)
#'
#' @rdname key-size
#' @export
key_size <- function(x) {
  UseMethod("key_size")
}

#' @export
key_size.tbl_ts <- function(x) {
  key_indices <- key_indices(x)
  if (is_empty(key_indices)) {
    return(NROW(x))
  }
  vapply(key_indices, length, integer(1))
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

#' @rdname key-size
#' @export
key_indices <- function(x) {
  UseMethod("key_indices")
}

#' @export
key_indices.tbl_ts <- function(x) {
  flat_keys <- flatten_key(key(x))
  grped_key <- grouped_df(x, flat_keys)
  attr(grped_key, "indices")
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
  not_tsibble(x)
  attr(x, "interval")
}

#' @rdname index-rd
#' @export
index <- function(x) {
  not_tsibble(x)
  attr(x, "index")
}

`index<-` <- function(x, value) {
  attr(x, "index") <- value
  x
}

#' `is_regular` checkes if a tsibble is spaced at regular time or not; `is_ordered`
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
build_tsibble <- function(
  x, key, index, groups = id(), regular = TRUE, validate = TRUE, ordered = NULL
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
  flat_keys <- flatten_key(key_vars)
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
  tbl_interval <- list()
  if (regular) {
    eval_idx <- eval_tidy(index, data = tbl)
    tbl_interval <- pull_interval(eval_idx, duplicated = TRUE)
  }

  # arrange in time order (by key and index)
  if (is.null(ordered)) { # first time to create a tsibble
    tbl <- tbl %>%
      arrange(!!! syms(flat_keys), !! index)
    ordered <- TRUE
  } else if (is_false(ordered)) { # false returns a warning
    if (is_empty(key_vars)) {
      msg <- sprintf(
        "The `tbl_ts` is not arranged by `%s` in ascending order.", idx_chr
      )
    } else {
      msg <- sprintf(
        "The `tbl_ts` is not arranged by `%s`, and `%s` in ascending order.", 
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
    "interval" = structure(tbl_interval, class = "interval"),
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

  flat_grps <- flatten_key(groups)
  grped_df <- grouped_df(tbl, flat_grps)
  tibble::new_tibble(
    grped_df,
    "vars" = structure(groups, class = "vars"),
    subclass = c("grouped_ts", "tbl_ts")
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
  enquos(...)
}

## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects.
extract_index_var <- function(data, index) {
  idx_type <- purrr::map_chr(data, index_sum)
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
  # identifiers <- c(flatten_key(key), idx)
  # below calls anyDuplicated.data.frame():
  # time zone associated with the index will be dropped, 
  # e.g. nycflights13::weather, thus result in duplicates. 
  # dup <- anyDuplicated(data[, identifiers, drop = FALSE])
  tbl_dup <- grouped_df(data, vars = flatten_key(key)) %>%
    summarise(zzz = anyDuplicated.default(!! index))
  if (any_not_equal_to_c(tbl_dup$zzz, 0)) {
    msg <- sprintf("Invalid tsibble: identical data entries from `%s`", idx)
    if (!is_empty(key)) {
      class(key) <- "key"
      msg <- sprintf("%s and %s.", msg, paste_comma(format(key)))
    } else {
      msg <- paste0(msg, ".")
    }
    msg <- paste(msg, "Use `inform_duplicates()` to check the duplicated rows.")
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
inform_duplicates <- function(data, key = id(), index, fromLast = FALSE) {
  use_id(key)
  index <- extract_index_var(data, enquo(index))

  grouped_df(data, vars = flatten_key(key)) %>%
    mutate(zzz = duplicated.default(!! index, fromLast = fromLast)) %>% 
    dplyr::pull(zzz)

  # identifiers <- c(flatten_key(key), quo_text2(index))
  # duplicated(data[, identifiers, drop = FALSE], fromLast = fromLast)
  # not handling time zone correctly for duplicated.data.frame
}
