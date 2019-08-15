globalVariables(c(".rows"))

#' Create a tsibble object
#'
#' \lifecycle{stable}
#'
#' @param ... A set of name-value pairs. The names of "key" and "index" should
#' be avoided as they are used as the arguments.
#' @param key Unquoted variable(s) that uniquely determine time indices. `NULL` for
#' empty key, and works with tidy selector (e.g. [dplyr::starts_with()]).
#' @param index A bare (or unquoted) variable to specify the time index variable.
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
#' # create a tsibble with one key
#' tsibble(
#'   qtr = rep(yearquarter("201001") + 0:9, 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30),
#'   key = group
#' )
#' @export
tsibble <- function(..., key = NULL, index, regular = TRUE, .drop = TRUE) {
  stopifnot(is_logical(regular, n = 1))
  dots <- list2(...)
  if (has_length(dots, 1) && is.data.frame(dots[[1]])) {
    abort("Must not be a data frame, do you want `as_tsibble()`?")
  }
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
#' # coerce tibble to tsibble with one key
#' # "date" is automatically considered as the index var, and "group" is the key
#' tbl2 <- tibble(
#'   mth = rep(yearmonth("2017-01") + 0:9, 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30)
#' )
#' as_tsibble(tbl2, key = group)
#' as_tsibble(tbl2, key = group, index = mth)
#' @export
as_tsibble <- function(x, key = NULL, index, regular = TRUE,
                       validate = TRUE, .drop = TRUE, ...) {
  stopifnot(is_logical(regular, n = 1))
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

#' @rdname as-tsibble
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

#' @rdname as-tsibble
#' @export
as_tsibble.list <- as_tsibble.tbl_df

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
#' library(dplyr)
#' pedestrian %>%
#'   group_by_key() %>%
#'   mutate(Hour_Since = Date_Time - min(Date_Time)) %>%
#'   update_tsibble(index = Hour_Since)
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
  if (is_true(regular)) {
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
  key_sym <- use_id(x, !!enquo(key))
  key_vars <- syms(unname(key_sym))

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
  if (is_false(is_empty(is_index_in_keys))) {
    abort(sprintf("Column `%s` can't be both index and key.", idx_chr[[1]]))
  }
  # arrange index from past to future for each key value
  if (vec_size(tbl) == 0 || is_null(ordered)) { # first time to create a tsibble
    tbl <- arrange(tbl, !!!key_vars, !!sym(index))
    ordered <- TRUE
  }
  if (!is_key_data) {
    key_data <- group_data(group_by(tbl, !!!key_vars, .drop = .drop))
  }
  if (is_false(ordered)) { # false returns a warning
    indices <- tbl[[index]]
    validate_order_lst <- function(x, .rows) {
      validate_order(x[.rows[[1]]])
    }
    actually_ordered <- summarise(key_data, 
      ".rows" := any(validate_order_lst(indices, .rows)))[[".rows"]]
    if (is_false(actually_ordered)) {
      idx_txt <- backticks(index)
      key_txt <- lapply(key_vars, expr_label)
      warn(sprintf(paste_inline(
        "Unspecified temporal ordering may yield unexpected results.",
        "Suggest to sort by %s first."
      ), comma(c(key_txt, idx_txt), sep = "")))
    }
    ordered <- actually_ordered
  }
  # validate tbl_ts
  if (validate) {
    tbl <- validate_tsibble(
      data = tbl, key = key_vars, key_data = key_data,
      index = index
    )
  }
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
#' @param index,index2 Strings for variable name.
#'
#' @keywords internal
#' @export
build_tsibble_meta <- function(x, key_data = NULL, index, index2,
                               ordered = NULL, interval = TRUE) {
  stopifnot(!is_null(ordered))
  tbl <- x
  attr(index, "ordered") <- ordered

  is_interval <- inherits(interval, "interval")
  msg_interval <- "Argument `interval` must be class interval, not %s."
  if (is_false(interval) || is_null(interval)) {
    interval <- irregular()
  } else if (is_true(interval)) {
    interval <- interval_pull(tbl[[index]])
  } else if (!is_interval) {
    abort(sprintf(msg_interval, class(interval)[1]))
  }
  if (unknown_interval(interval) && (vec_size(tbl) > vec_size(key_data))) {
    warn("Can't obtain the interval, please see `?tsibble` for details.")
  }

  idx_lgl <- index == index2
  # convert grouped_df to tsibble:
  # the `groups` arg must be supplied, otherwise returns a `tbl_ts` not grouped
  if (!idx_lgl) {
    tbl <- group_by(tbl, !!sym(index2), add = TRUE)
  }
  grp_data <- tbl %@% "groups"
  tbl <- new_tibble(tbl,
    "key" = key_data, "index" = index, "index2" = index2,
    "interval" = interval, "groups" = NULL, nrow = vec_size(tbl),
    class = "tbl_ts")
  is_grped <- is_grouped_df(x) || !idx_lgl
  if (is_grped) {
    cls <- c("grouped_ts", "grouped_df")
    tbl <- new_tsibble(tbl, "groups" = grp_data, class = cls)
  }
  tbl
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
    x
  } else if (all(x < 0)) {
    TRUE
  } else if ((vec_duplicate_any(x)) > 0) {
    abort(sprintf("Duplicated indices: %s", comma(x[vec_duplicate_detect(x)])))
  } else {
    is_ascending(x, na.rm = TRUE, strictly = TRUE)
  }
}

# check if a comb of key vars result in a unique data entry
# if TRUE return the data, otherwise raise an error
validate_tsibble <- function(data, key, index, key_data = NULL) {
  is_dup <- duplicated_key_index(data, key, index, key_data)
  if (is_dup) {
    header <- "A valid tsibble must have distinct rows identified by key and index."
    hint <- "Please use `duplicates()` to check the duplicated rows."
    abort(paste_inline(header, hint))
  }
  data
}

# used for column-verbs to check if the tsibble holds
retain_tsibble <- function(data, key, index) {
  is_dup <- duplicated_key_index(data, key, index)
  if (is_dup) {
    header <- "The result is not a valid tsibble."
    hint <- "Do you need `as_tibble()` to work with data frame?"
    abort(paste_inline(header, hint))
  }
  data
}

#' @export
format.tbl_ts <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  is_index_null(x)
  if (!is_null(x %@% "regular") || !is_null(x %@% "ordered")) {
    warn("`.data`. is a corrupt tsibble object, please reconstruct with `as_tsibble()`.")
  }
  format(trunc_mat(x, n = n, width = width, n_extra = n_extra))
}

#' Coerce to a tibble or data frame
#'
#' @param x A `tbl_ts`.
#' @param ... Ignored.
#' @inheritParams base::as.data.frame
#'
#' @rdname as-tibble
#' @export
#' @examples
#' as_tibble(pedestrian)
as_tibble.tbl_ts <- function(x, ...) {
  x <- remove_tsibble_attrs(x)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}

as_tibble.grouped_ts <- function(x, ...) {
  x <- remove_tsibble_attrs(x)
  class(x) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  x
}

as_tibble.grouped_df <- function(x, ...) {
  x
}

#' @rdname as-tibble
#' @export
as.data.frame.tbl_ts <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- as_tibble(x)
  NextMethod()
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
  key <- use_id(data, !!enquo(key))
  index <- sym(validate_index(data, !!enquo(index)))
  duplicated_key_index(data, key = key, index = index)
}

#' @param from_last `TRUE` does the duplication check from the last of identical
#' elements.
#'
#' @rdname duplicates
#' @export
are_duplicated <- function(data, key = NULL, index, from_last = FALSE) {
  key <- use_id(data, !!enquo(key))
  index <- sym(validate_index(data, !!enquo(index)))
  res <-
    mutate(
      grouped_df(data, vars = key),
      !!"zzz" := duplicated.default(!!index, fromLast = from_last)
    )
  res$zzz
}

#' @rdname duplicates
#' @export
duplicates <- function(data, key = NULL, index) {
  key <- use_id(data, !!enquo(key))
  index <- sym(validate_index(data, !!enquo(index)))
  ungroup(filter(grouped_df(data, vars = key), vec_duplicate_detect(!!index)))
}

duplicated_key_index <- function(data, key, index, key_data = NULL) {
  # NOTE: bug in anyDuplicated.data.frame() (fixed in R 3.5.0)
  # identifiers <- c(key, idx)
  # below calls anyDuplicated.data.frame():
  # time zone associated with the index will be dropped,
  # e.g. nycflights13::weather, thus result in duplicates.
  # dup <- anyDuplicated(data[, identifiers, drop = FALSE])
  if (is_null(key_data)) {
    keyed_data <- grouped_df(as_tibble(data), key)
  } else {
    keyed_data <- new_grouped_df(data, groups = key_data)
  }
  res <- summarise(keyed_data, !!"zzz" := vec_duplicate_any(!!sym(index)))
  any(res$zzz > 0)
}

remove_tsibble_attrs <- function(x) {
  attr(x, "key") <- attr(x, "index") <- attr(x, "index2") <- NULL
  # attr(x, "interval") <- NULL
  attr(x, "interval") <- attr(x, "ordered") <- attr(x, "regular") <- NULL # will be removed
  x
}

assert_key_data <- function(x) {
  nc <- NCOL(x)
  if (is_false(
    is.data.frame(x) &&
    nc > 0 &&
    is.list(x[[nc]]) &&
    names(x)[[nc]] == ".rows"
  )) {
    abort("The `key` attribute must be a data frame with its last column called `.rows`.")
  }
}
