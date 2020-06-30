globalVariables(c(".rows", "id"))

#' Create a tsibble object
#'
#' \lifecycle{stable}
#'
#' @param ... A set of name-value pairs.
#' @param key Variable(s) that uniquely determine time indices. `NULL` for
#' empty key, and `c()` for multiple variables. It works with tidy selector
#' (e.g. [dplyr::starts_with()]).
#' @param index A variable to specify the time index variable.
#' @param regular Regular time interval (`TRUE`) or irregular (`FALSE`). The
#' interval is determined by the greatest common divisor of index column, if `TRUE`.
#' @param .drop If `TRUE`, empty key groups are dropped.
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
#' # create a tsibble w/o a key
#' tsibble(
#'   date = as.Date("2017-01-01") + 0:9,
#'   value = rnorm(10)
#' )
#'
#' # create a tsibble with a single variable for key
#' tsibble(
#'   qtr = rep(yearquarter("2010 Q1") + 0:9, 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30),
#'   key = group
#' )
#'
#' # create a tsibble with multiple variables for key
#' tsibble(
#'   mth = rep(yearmonth("2010 Jan") + 0:8, each = 3),
#'   xyz = rep(c("x", "y", "z"), each = 9),
#'   abc = rep(letters[1:3], times = 9),
#'   value = rnorm(27),
#'   key = c(xyz, abc)
#' )
#'
#' # create a tsibble containing "key" and "index" as column names
#' tsibble(!!!list(
#'   index = rep(yearquarter("2010 Q1") + 0:9, 3),
#'   key = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30)),
#'   key = key, index = index
#' )
#' @export
tsibble <- function(..., key = NULL, index, regular = TRUE, .drop = TRUE) {
  stopifnot(is_logical(regular, n = 1))
  dots <- list2(...)
  tbl <- tibble(!!!dots)
  index <- enquo(index)
  build_tsibble(tbl,
    key = !!enquo(key), index = !!index, interval = regular,
    .drop = .drop
  )
}

#' Coerce to a tsibble object
#'
#' \lifecycle{stable}
#'
#' @param x Other objects to be coerced to a tsibble (`tbl_ts`).
#' @inheritParams tsibble
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables leads to unique time indices (i.e. a valid tsibble). If you
#' are sure that it's a valid input, specify `FALSE` to skip the checks.
#' @param ... Other arguments passed on to individual methods.
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
#' @rdname as-tsibble
#' @seealso [tsibble]
#'
#' @examples
#' # coerce tibble to tsibble w/o a key
#' tbl1 <- tibble(
#'   date = as.Date("2017-01-01") + 0:9,
#'   value = rnorm(10)
#' )
#' as_tsibble(tbl1)
#' # supply the index to suppress the message
#' as_tsibble(tbl1, index = date)
#'
#' # coerce tibble to tsibble with a single variable for key
#' # use `yearquarter()` to represent quarterly data
#' tbl2 <- tibble(
#'   qtr = rep(yearquarter("2010 Q1") + 0:9, 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30)
#' )
#' # "qtr" is automatically considered as the index var
#' as_tsibble(tbl2, key = group)
#' as_tsibble(tbl2, key = group, index = qtr)
#'
#' # create a tsibble with multiple variables for key
#' # use `yearmonth()` to represent monthly data
#' tbl3 <- tibble(
#'   mth = rep(yearmonth("2010 Jan") + 0:8, each = 3),
#'   xyz = rep(c("x", "y", "z"), each = 9),
#'   abc = rep(letters[1:3], times = 9),
#'   value = rnorm(27)
#' )
#' as_tsibble(tbl3, key = c(xyz, abc))
#' @export
as_tsibble <- function(x, key = NULL, index, regular = TRUE,
                       validate = TRUE, .drop = TRUE, ...) {
  stopifnot(is_logical(regular, n = 1))
  check_dots_used()
  UseMethod("as_tsibble")
}

#' @keywords internal
#' @export
as_tsibble.tbl_df <- function(x, key = NULL, index, regular = TRUE,
                              validate = TRUE, .drop = TRUE, ...) {
  index <- enquo(index)
  build_tsibble(x,
    key = !!enquo(key), index = !!index, interval = regular,
    validate = validate, .drop = .drop
  )
}

#' @keywords internal
#' @export
as_tsibble.data.frame <- as_tsibble.tbl_df

#' @keywords internal
#' @export
as_tsibble.tbl_ts <- function(x, ...) {
  update_tsibble(x, ...)
}

#' @keywords internal
#' @export
as_tsibble.tbl <- as_tsibble.tbl_df

#' @keywords internal
#' @export
as_tsibble.grouped_df <- function(x, key = NULL, index, regular = TRUE,
                                  validate = TRUE, .drop = key_drop_default(x),
                                  ...) {
  index <- enquo(index)
  build_tsibble(x,
    key = !!enquo(key), index = !!index, interval = regular,
    validate = validate, .drop = .drop
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

#' Update key and index for a tsibble
#'
#' @param x A tsibble.
#' @inheritParams as_tsibble
#' @details
#' Unspecified arguments will inherit the attributes from `x`.
#' @export
#' @examples
#' # update index
#' library(dplyr)
#' pedestrian %>%
#'   group_by_key() %>%
#'   mutate(Hour_Since = Date_Time - min(Date_Time)) %>%
#'   update_tsibble(index = Hour_Since)
#'
#' # update key: drop the variable "State" from the key
#' tourism %>% 
#'   update_tsibble(key = c(Purpose, Region))
update_tsibble <- function(x, key, index, regular = is_regular(x),
                           validate = TRUE, .drop = key_drop_default(x)) {
  not_tsibble(x)
  stopifnot(is_logical(regular, n = 1))

  key <- enquo(key)
  if (quo_is_missing(key)) {
    key <- key_vars(x)
  }
  idx <- enquo(index)
  if (quo_is_missing(idx)) {
    idx <- index_var(x)
  }
  if (regular) {
    int <- interval(x)
    int <- if (unknown_interval(int)) int else TRUE
  } else {
    int <- FALSE
  }

  is_idx_idx2 <- identical(index_var(x), index2_var(x))
  build_tsibble(
    as_tibble(x),
    key = !!key, index = !!idx,
    index2 = if (is_idx_idx2) !!idx else !!index2(x),
    interval = int, validate = validate, .drop = .drop
  )
}

## tsibble is a special class of tibble that handles temporal data. It
## requires a sequence of time index to be unique across every identifier.

#' Low-level constructor for a tsibble object
#'
#' @description
#' `build_tsibble()` creates a `tbl_ts` object with more controls. It is useful
#' for creating a `tbl_ts` internally inside a function, and it allows developers to
#' determine if the time needs ordering and the interval needs calculating.
#'
#' @param x A `data.frame`, `tbl_df`, `tbl_ts`, or other tabular objects.
#' @inheritParams as_tsibble
#' @param key_data A data frame containing key variables and `.rows`. When a data
#' frame is supplied, the argument `key` will be ignored.
#' @param index2 A candidate of `index` to update the index to a new one when
#' [index_by]. By default, it's identical to `index`.
#' @param ordered The default of `NULL` arranges the key variable(s) first and
#' then index from past to future. `TRUE` suggests to skip the ordering as `x` in
#' the correct order. `FALSE` checks the ordering and may give a warning.
#' @param interval `TRUE` automatically calculates the interval, and `FALSE` for
#' irregular interval. Use the specified interval via [new_interval()] as is.
#'
#' @export
#' @examples
#' # Prepare `pedestrian` to use a new index `Date` ----
#' pedestrian %>%
#'   build_tsibble(
#'     key = !!key_vars(.), index = !!index(.), index2 = Date,
#'     interval = interval(.)
#'   )
build_tsibble <- function(x, key = NULL, key_data = NULL, index, index2 = index,
                          ordered = NULL, interval = TRUE, validate = TRUE,
                          .drop = key_drop_default(x)) {
  is_key_data <- !is_null(key_data)
  if (is_key_data) {
    assert_key_data(key_data)
    key <- head(names(key_data), -1L)
  }
  key_vars <- names(eval_select(enquo(key), data = x))

  tbl <- as_tibble(x)
  # extract or pass the index var
  qindex <- enquo(index)
  index <- validate_index(tbl, !!qindex)
  # if index2 not specified
  index2 <- enquo(index2)
  if (identical(qindex, index2)) {
    index2 <- index
  } else {
    index2 <- validate_index(tbl, !!index2)
  }
  # index cannot be part of the keys
  idx_chr <- c(index, index2)
  is_index_in_keys <- intersect(idx_chr, key_vars)
  if (!is_empty(is_index_in_keys)) {
    abort(sprintf("Column `%s` can't be both index and key.", idx_chr[[1]]))
  }
  # arrange index from past to future for each key value
  if (vec_size(tbl) == 0 || is_null(ordered)) { # first time to create a tsibble
    tbl <- arrange(tbl, !!!syms(key_vars), !!sym(index))
    ordered <- TRUE
  }
  if (!is_key_data) {
    key_data <- group_data(grouped_df(tbl, vars = key_vars, drop = .drop))
  }
  if (!ordered) { # if false, double check
    ordered <- validate_index_order(tbl, key_data, index)
  }
  # validate tbl_ts
  if (validate) {
    tbl <- validate_tsibble(
      data = tbl, key = key_vars, key_data = key_data,
      index = index
    )
  }
  interval <- validate_interval(tbl, key_data, index, interval)
  build_tsibble_meta(tbl,
    key_data = key_data, index = index, index2 = index2,
    ordered = ordered, interval = interval
  )
}

#' Low-level & high-performance constructor for a tsibble object
#'
#' @description
#' \lifecycle{experimental}
#'
#' `build_tsibble_meta()` does much less checks than `build_tsibble()` for
#' high performance.
#'
#' @inheritParams build_tsibble
#' @param index,index2 Quoted variable name.
#'
#' @keywords internal
#' @export
build_tsibble_meta <- function(x, key_data = NULL, index, index2,
                               ordered = NULL, interval = NULL) {
  stopifnot(!is_null(ordered))
  stopifnot(inherits(interval, "interval"))
  attr(index, "ordered") <- ordered
  idx_lgl <- index != index2
  is_grped <- is_grouped_df(x) || idx_lgl

  # convert grouped_df to tsibble:
  # the `groups` arg must be supplied, otherwise returns a `tbl_ts` not grouped
  if (idx_lgl) {
    x <- grouped_df(x, vars = union(group_vars(x), index2))
  }
  grp_data <- x %@% "groups"
  x <- new_tibble(x,
    "key" = key_data, "index" = index, "index2" = index2,
    "interval" = interval, "groups" = NULL, nrow = vec_size(x),
    class = "tbl_ts")
  if (is_grped) {
    cls <- c("grouped_ts", "grouped_df")
    x <- new_tsibble(x, "groups" = grp_data, class = cls)
  }
  x
}

#' Create a subclass of a tsibble
#'
#' @param x A `tbl_ts`, required.
#' @param ... Name-value pairs defining new attributes other than a tsibble.
#' @param class Subclasses to assign to the new object, default: none.
#'
#' @export
new_tsibble <- function(x, ..., class = NULL) {
  not_tsibble(x)
  x <- new_tibble(x, ..., nrow = vec_size(x), class = "tbl_ts")
  assert_key_data(attr(x, "key"))
  class(x) <- c(class, class(x))
  x
}

## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects.
validate_index <- function(data, index) {
  val_idx <- vapply(data, index_valid, logical(1))
  index <- enquo(index)
  if (quo_is_null(index)) abort("Argument `index` must not be `NULL`.")

  names <- names2(data)
  if (quo_is_missing(index)) {
    if (sum(val_idx, na.rm = TRUE) != 1) {
      abort("Can't determine index and please specify argument `index`.")
    }
    chr_index <- names[val_idx]
    chr_index <- chr_index[!is.na(chr_index)]
    inform(sprintf("Using `%s` as index variable.", chr_index))
  } else {
    chr_index <- vars_pull(names, !!index)
    idx_pos <- vec_in(names, chr_index)
    val_lgl <- val_idx[idx_pos]
    if (is.na(val_lgl)) {
      return(chr_index)
    } else if (!val_lgl) {
      cls_idx <- vapply(data, function(x) class(x)[1], character(1))
      abort(sprintf("Unsupported index type: %s", cls_idx[idx_pos]))
    }
  }
  if (anyNA(data[[chr_index]])) {
    abort(sprintf("Column `%s` (index) must not contain `NA`.", chr_index))
  }
  chr_index
}

validate_order <- function(x) {
  if (is_bare_logical(x)) {
    any(x)
  } else if (is_bare_numeric(x) && all(x < 0)) {
    TRUE
  } else if (vec_duplicate_any(x) > 0) {
    abort(sprintf("Duplicated indices: %s", comma(x[vec_duplicate_detect(x)])))
  } else {
    is_ascending(x, na.rm = TRUE, strictly = TRUE)
  }
}

validate_index_order <- function(data, key_data, index) {
  indices <- data[[index]]
  validate_order_lst <- function(x, .rows) {
    validate_order(x[.rows[[1]]])
  }
  ordered <- summarise(key_data,
    ".rows" := any(validate_order_lst(indices, .rows)))[[".rows"]]
  if (!ordered) {
    idx_txt <- backticks(index)
    key_txt <- backticks(head(names(key_data), -1L))
    warn(c(
      "Current temporal ordering may yield unexpected results.",
      i = sprintf("Suggest to sort by %s first.", comma(c(key_txt, idx_txt), sep = ""))
    ))
  }
  ordered
}

validate_interval <- function(data, key_data, index, interval) {
  nrows <- vec_size(data)
  is_interval <- inherits(interval, "interval")
  msg_interval <- "Argument `interval` must be class interval, not %s."
  if (is_false(interval) || is_null(interval)) {
    interval <- irregular()
  } else if (nrows == 0) {
    interval <- new_interval()
  } else if (is_true(interval)) {
    interval <- interval_pull(data[[index]])
  } else if (!is_interval) {
    abort(sprintf(msg_interval, class(interval)[1]))
  }
  if (unknown_interval(interval) && (nrows > vec_size(key_data))) {
    abort(c(
      "Can't obtain the interval due to the mismatched index class.",
      i = "Please see `vignette(\"FAQ\")` for details."
    ))
  }
  interval
}

# check if a comb of key vars result in a unique data entry
# if TRUE return the data, otherwise raise an error
validate_tsibble <- function(data, key, index, key_data = NULL) {
  is_dup <- duplicated_key_index(data, key, index, key_data)
  if (is_dup) {
    abort(c(
      "A valid tsibble must have distinct rows identified by key and index.", 
      i = "Please use `duplicates()` to check the duplicated rows."))
  }
  data
}

# used for column-verbs to check if the tsibble holds
retain_tsibble <- function(data, key, index) {
  is_dup <- duplicated_key_index(data, key, index)
  if (is_dup) {
    abort(c(
      "The result is not a valid tsibble.",
      i = "Do you need `as_tibble()` to work with data frame?"))
  }
  data
}

#' @export
format.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  is_index_null(x)
  if (!is_null(x %@% "regular") || !is_null(x %@% "ordered")) {
    warn(c(
      "`.data`. is a corrupt tsibble object.",
      i = "Please reconstruct with `as_tsibble()`."))
  }
  format(trunc_mat(x, n = n, width = width, n_extra = n_extra))
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
as_tibble.tbl_ts <- function(x, ...) {
  new_tibble(vec_data(x), nrow = nrow(x))
}

as_tibble.grouped_ts <- function(x, ...) {
  new_tibble(vec_data(x), nrow = nrow(x),
    groups = group_data(x), class = "grouped_df")
}

as_tibble.grouped_df <- function(x, ...) {
  x
}

#' @keywords internal
#' @export
as.data.frame.tbl_ts <- function(x, row.names = NULL, optional = FALSE, ...) {
  new_data_frame(vec_data(x))
}

#' Test duplicated observations determined by key and index variables
#'
#' @description
#' \lifecycle{stable}
#'
#' * `is_duplicated()`: a logical scalar if the data exist duplicated observations.
#' * `are_duplicated()`: a logical vector, the same length as the row number of `data`.
#' * `duplicates()`: identical key-index data entries.
#'
#' @param data A data frame for creating a tsibble.
#' @inheritParams tsibble
#'
#' @rdname duplicates
#' @export
#' @examples
#' harvest <- tibble(
#'   year = c(2010, 2011, 2013, 2011, 2012, 2014, 2014),
#'   fruit = c(rep(c("kiwi", "cherry"), each = 3), "cherry"),
#'   kilo = sample(1:10, size = 7)
#' )
#' is_duplicated(harvest, key = fruit, index = year)
#' are_duplicated(harvest, key = fruit, index = year)
#' are_duplicated(harvest, key = fruit, index = year, from_last = TRUE)
#' duplicates(harvest, key = fruit, index = year)
is_duplicated <- function(data, key = NULL, index) {
  key <- names(eval_select(enquo(key), data = data))
  index <- sym(validate_index(data, !!enquo(index)))
  duplicated_key_index(data, key = key, index = index)
}

#' @param from_last `TRUE` does the duplication check from the last of identical
#' elements.
#'
#' @rdname duplicates
#' @export
are_duplicated <- function(data, key = NULL, index, from_last = FALSE) {
  key <- names(eval_select(enquo(key), data = data))
  index <- sym(validate_index(data, !!enquo(index)))
  res <-
    mutate(
      grouped_df(data, vars = key, drop = TRUE),
      !!"zzz" := duplicated.default(!!index, fromLast = from_last)
    )
  res$zzz
}

#' @rdname duplicates
#' @export
duplicates <- function(data, key = NULL, index) {
  key <- names(eval_select(enquo(key), data = data))
  index <- sym(validate_index(data, !!enquo(index)))
  grped_df <- grouped_df(data, vars = key, drop = TRUE)
  ungroup(filter(grped_df, vec_duplicate_detect(!!index)))
}

duplicated_key_index <- function(data, key, index, key_data = NULL) {
  if (is_null(key_data)) {
    keyed_data <- grouped_df(as_tibble(data), key, drop = TRUE)
  } else {
    keyed_data <- new_grouped_df(data, groups = key_data)
  }
  res <- summarise(keyed_data, !!"zzz" := vec_duplicate_any(!!sym(index)))
  any(res$zzz > 0)
}

assert_key_data <- function(x) {
  nc <- length(x)
  if (is_false(
    is.data.frame(x) &&
    nc > 0 &&
    is.list(x[[nc]]) &&
    names(x)[[nc]] == ".rows"
  )) {
    abort("The `key` attribute must be a data frame with its last column called `.rows`.")
  }
}
