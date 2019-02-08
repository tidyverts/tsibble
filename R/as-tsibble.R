#' Create a tsibble object
#'
#' @param ... A set of name-value pairs. The names of "key" and "index" should
#' be avoided as they are used as the arguments.
#' @param key Variable(s) that define unique time indices, used in conjunction
#' with the helper [id()]. If a univariate time series (without an explicit key),
#' simply call `id()`.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param regular Regular time interval (`TRUE`) or irregular (`FALSE`). The
#' interval is determined by the greatest common divisor of index column, if `TRUE`.
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
#' tsibble(
#'   date = as.Date("2017-01-01") + 0:9,
#'   value = rnorm(10)
#' )
#'
#' # create a tsibble with one key ----
#' tsibble(
#'   qtr = rep(yearquarter("201001") + 0:9, 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30),
#'   key = id(group)
#' )
#'
#' @export
tsibble <- function(..., key = id(), index, regular = TRUE) {
  dots <- list2(...)
  if (has_length(dots, 1) && is.data.frame(dots[[1]])) {
    abort("Must not be a data frame, do you want `as_tsibble()`?")
  }
  tbl <- tibble(!!! dots)
  index <- enquo(index)
  build_tsibble(tbl, key = !! enquo(key), index = !! index, regular = regular)
}

#' Coerce to a tsibble object
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
#' # coerce tibble to tsibble w/o a key ----
#' tbl1 <- tibble(
#'   date = as.Date("2017-01-01") + 0:9,
#'   value = rnorm(10)
#' )
#' as_tsibble(tbl1)
#' # specify the index var
#' as_tsibble(tbl1, index = date)
#'
#' # coerce tibble to tsibble with one key ----
#' # "date" is automatically considered as the index var, and "group" is the key
#' tbl2 <- tibble(
#'   mth = rep(yearmonth("2017-01") + 0:9, 3),
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
  x, key = id(), index, regular = TRUE, validate = TRUE, ...
) {
  index <- enquo(index)
  build_tsibble(
    x, key = !! enquo(key), index = !! index,
    regular = regular, validate = validate
  )
}

#' @keywords internal
#' @export
as_tsibble.grouped_ts <- function(x, ...) {
  ungroup(x)
}

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

#' Return measured variables
#'
#' @param x A `tbl_ts`.
#'
#' @rdname measured-vars
#' @examples
#' measures(pedestrian)
#' measures(tourism)
#'
#' measured_vars(pedestrian)
#' measured_vars(tourism)
#' @export
measures <- function(x) {
  UseMethod("measures")
}

#' @export
measures.tbl_ts <- function(x) {
  syms(measured_vars(x))
}

#' @rdname measured-vars
#' @export
measured_vars <- function(x) {
  UseMethod("measured_vars")
}

#' @export
measured_vars.tbl_ts <- function(x) {
  all_vars <- dplyr::tbl_vars(x)
  key_vars <- key_vars(x)
  idx_var <- index_var(x)
  setdiff(all_vars, c(key_vars, idx_var))
}

#' Return index variable from a tsibble
#'
#' @param x A tsibble object.
#' @rdname index-rd
#' @examples
#' index(pedestrian)
#' index_var(pedestrian)
#' @export
index <- function(x) {
  not_tsibble(x)
  x %@% index
}

#' @rdname index-rd
#' @export
index_var <- function(x) {
  as_string(index(x))
}

#' @rdname index-rd
#' @export
index2 <- function(x) {
  not_tsibble(x)
  x %@% index2
}

#' @rdname index-rd
#' @export
index2_var <- function(x) {
  as_string(index2(x))
}

#' Meta-information of a tsibble
#'
#' * `interval()` returns an interval of a tsibble.
#' * `is_regular` checks if a tsibble is spaced at regular time or not.
#' * `is_ordered` checks if a tsibble is ordered by key and index.
#'
#' @param x A tsibble object.
#' @rdname regular
#' @examples
#' interval(pedestrian)
#' @export
interval <- function(x) {
  not_tsibble(x)
  x %@% interval
}

#' @rdname regular
#' @examples
#' is_regular(pedestrian)
#' is_ordered(pedestrian)
#' @export
is_regular <- function(x) {
  not_tsibble(x)
  x %@% regular
}

#' @rdname regular
#' @export
is_ordered <- function(x) {
  not_tsibble(x)
  x %@% ordered
}

#' If the object is a tsibble
#'
#' @param x An object.
#'
#' @return TRUE if the object inherits from the tbl_ts class.
#' @rdname is-tsibble
#' @aliases is.tsibble
#' @examples
#' # A tibble is not a tsibble ----
#' tbl <- tibble(
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

## tsibble is a special class of tibble that handles temporal data. It
## requires a sequence of time index to be unique across every identifier.

#' Low-level constructor for a tsibble object
#'
#' `build_tsibble()` creates a `tbl_ts` object with more controls. It is useful
#' for creating a `tbl_ts` internally inside a function, and it allows developers to
#' determine if the time needs ordering and the interval needs calculating.
#'
#' @param x A `data.frame`, `tbl_df`, `tbl_ts`, or other tabular objects.
#' @inheritParams as_tsibble
#' @param index2 A candidate of `index` to update the index to a new one when
#' [index_by]. By default, it's identical to `index`.
#' @param ordered The default of `NULL` arranges the key variable(s) first and
#' then index from past to future. `TRUE` suggests to skip the ordering as `x` in
#' the correct order. `FALSE` also skips the ordering but gives a warning instead.
#' @param interval `NULL` computes the interval. Use the specified interval via
#' [new_interval()] as is, if an class of `interval` is supplied.
#'
#' @export
#' @examples
#' # Prepare `pedestrian` to use a new index `Date` ----
#' pedestrian %>%
#'   build_tsibble(
#'     key = key(.), index = !! index(.), index2 = Date, interval = interval(.)
#'   )
build_tsibble <- function(
  x, key, index, index2, ordered = NULL, regular = TRUE, interval = NULL,
  validate = TRUE
) {
  # if key is quosures
  key_sym <- use_id(x, !! enquo(key))
  is_key_data <- is.data.frame(key_sym)
  if (is_key_data) {
    key_data <- key_sym
    key_sym <- head(names(key_data), -1L)
  }

  is_grped <- dplyr::is_grouped_df(x) || is_grouped_ts(x)
  if (is_grped) {
    tbl <- x
  } else {
    tbl <- as_tibble(x)
  }

  # extract or pass the index var
  qindex <- enquo(index)
  index <- validate_index(tbl, qindex)
  # if index2 not specified
  index2 <- enquo(index2)
  if (quo_is_missing(index2)) {
    index2 <- index
  } else if (identical(qindex, index2)) {
    index2 <- get_expr(index2)
  } else {
    index2 <- validate_index(tbl, index2)
  }
  # (1) validate and process key vars (from expr to a list of syms)
  key_vars <- validate_key(tbl, key_sym)
  # (2) index cannot be part of the keys
  idx_chr <- c(as_string(index), as_string(index2))
  is_index_in_keys <- intersect(idx_chr, key_vars)
  if (is_false(is_empty(is_index_in_keys))) {
    abort(sprintf("Column `%s` can't be both index and key.", idx_chr[[1]]))
  }
  # arrange in time order (by key and index)
  if (is_null(ordered)) { # first time to create a tsibble
    tbl <- arrange(tbl, !!! key_vars, !! index)
    ordered <- TRUE
  } else if (is_false(ordered)) { # false returns a warning
    msg_header <- "Unexpected temporal ordering. Please sort by %s."
    idx_txt <- expr_text(index)
    if (is_empty(key)) {
      warn(sprintf(msg_header, idx_txt))
    } else {
      warn(sprintf(msg_header, paste_comma(c(key_vars, idx_txt))))
    }
  } # true do nothing
  # validate tbl_ts
  if (!is_key_data) {
    key_vars <- unname(key_vars)
    key_data <- group_data(group_by(tbl, !!! key_vars))
  }
  if (validate) {
    tbl <- validate_tsibble(data = tbl, key = key_vars, index = index)
  }
  build_tsibble_meta(
    tbl, key = key_data, index = !! index, index2 = !! index2,
    regular = regular, ordered = ordered, interval = interval
  )
}

build_tsibble_meta <- function(
  x, key, index, index2, ordered = NULL, regular = TRUE, interval = NULL
) {
  if (is_null(regular)) abort("Argument `regular` must not be `NULL`.")

  index <- get_expr(enquo(index))
  index2 <- enquo(index2)
  if (quo_is_missing(index2)) {
    index2 <- index
  } else {
    index2 <- get_expr(index2)
  }
  is_grped <- dplyr::is_grouped_df(x) || is_grouped_ts(x)
  tbl <- as_tibble(x)

  if (NROW(x) == 0) {
    if (is_false(regular)) {
      interval <- irregular()
    } else {
      interval <- init_interval()
    }
    grp_data <- group_data(tbl)
    tbl <- new_tibble(
      tbl, "key" = key, "index" = index, "index2" = index2,
      "interval" = interval, "regular" = regular, "ordered" = TRUE,
      "groups" = NULL, nrow = 0L, class = "tbl_ts"
    )
    if (is_grped) {
      tbl <- new_tsibble(tbl, "groups" = grp_data, class = "grouped_ts")
    }
    return(tbl)
  }

  if (is_false(regular)) {
    interval <- irregular()
  } else if (regular && is_null(interval)) {
    eval_idx <- eval_tidy(index, data = tbl)
    interval <- pull_interval(eval_idx)
  } else if (is_false(inherits(interval, "interval"))) {
    abort(sprintf(
      "Argument `interval` must be class interval, not %s.",
      class(interval)[1]
    ))
  }

  idx_lgl <- identical(index, index2)
  # convert grouped_df to tsibble:
  # the `groups` arg must be supplied, otherwise returns a `tbl_ts` not grouped
  if (!idx_lgl) {
    tbl <- group_by(tbl, !! index2, add = TRUE)
  }
  grp_data <- group_data(tbl)
  tbl <- new_tibble(
    tbl, "key" = key, "index" = index, "index2" = index2,
    "interval" = interval, "regular" = regular, "ordered" = ordered,
    "groups" = NULL, nrow = NROW(tbl), class = "tbl_ts"
  )
  if (is_grped) {
    tbl <- new_tsibble(tbl, "groups" = grp_data, class = "grouped_ts")
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
  x <- new_tibble(x, ..., class = "tbl_ts")
  assert_key_data(x %@% key)
  attr(x, "row.names") <- .set_row_names(NROW(x))
  class(x) <- c(class, class(x))
  x
}

#' Identifiers used for creating key
#'
#' @param ... Variables passed to tsibble()/as_tsibble().
#'
#' @keywords internal
#' @export
id <- function(...) {
  unname(enexprs(...))
}

## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects.
validate_index <- function(data, index) {
  val_idx <- map_lgl(data, index_valid)
  if (quo_is_null(index)) {
    abort("Argument `index` must not be `NULL`.")
  }
  if (quo_is_missing(index)) {
    if (sum(val_idx, na.rm = TRUE) != 1) {
      abort("Can't determine index and please specify argument `index`.")
    }
    chr_index <- names(data)[val_idx]
    chr_index <- chr_index[!is.na(chr_index)]
    inform(sprintf("Using `%s` as index variable.", chr_index))
  } else {
    chr_index <- tidyselect::vars_pull(names(data), !! index)
    idx_pos <- names(data) %in% chr_index
    val_lgl <- val_idx[idx_pos]
    if (is.na(val_lgl)) {
      return(sym(chr_index))
    } else if (!val_idx[idx_pos]) {
      cls_idx <- map_chr(data, ~ class(.)[1])
      abort(sprintf("Unsupported index type: %s", cls_idx[idx_pos]))
    }
  }
  if (anyNA(data[[chr_index]])) {
    abort(sprintf("Column `%s` (index) must not contain `NA`.", chr_index))
  }
  sym(chr_index)
}

duplicated_key_index <- function(data, key, index) {
  # NOTE: bug in anyDuplicated.data.frame() (fixed in R 3.5.0)
  # identifiers <- c(key, idx)
  # below calls anyDuplicated.data.frame():
  # time zone associated with the index will be dropped,
  # e.g. nycflights13::weather, thus result in duplicates.
  # dup <- anyDuplicated(data[, identifiers, drop = FALSE])
  res <- 
    summarise(
      grouped_df(as_tibble(data), key),
      !! "zzz" := anyDuplicated.default(!! index)
    )
  any_not_equal_to_c(res$zzz, 0)
}

# check if a comb of key vars result in a unique data entry
# if TRUE return the data, otherwise raise an error
validate_tsibble <- function(data, key, index) {
  is_dup <- duplicated_key_index(data, key, index)
  if (is_dup) {
    header <- "A valid tsibble must have distinct rows identified by key and index.\n"
    hint <- "Please use `duplicates()` to check the duplicated rows."
    abort(paste0(header, hint))
  }
  data
}

# used for column-verbs to check if the tsibble holds
retain_tsibble <- function(data, key, index) {
  is_dup <- duplicated_key_index(data, key, index)
  if (is_dup) {
    header <- "The result is not a valid tsibble.\n"
    hint <- "Do you need `as_tibble()` to work with data frame?"
    abort(paste0(header, hint))
  }
  data
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
#'
#' # a grouped tbl_ts -----
#' grped_ped <- pedestrian %>% group_by(Sensor)
#' as_tibble(grped_ped)
as_tibble.tbl_ts <- function(x, ...) {
  x <- remove_tsibble_attrs(x)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  as_tibble(x, ...)
}

as_tibble.grouped_ts <- function(x, ...) {
  as_grouped_df(x)
}

as_tibble.grouped_df <- function(x, ...) {
  x
}

#' @keywords internal
#' @export
as_tibble.lst_ts <- function(x, ...) {
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}

#' @keywords internal
#' @export
as.data.frame.lst_ts <- function(x, ...) {
  class(x) <- "data.frame"
  x
}

#' @rdname as-tibble
#' @export
as.data.frame.tbl_ts <- function(x, row.names = NULL, optional = FALSE, ...) {
  x <- as_tibble(x)
  NextMethod()
}

use_id <- function(x, key) {
  key <- enquo(key)
  if (quo_is_call(key)) {
    call_fn <- call_name(key)
    if (call_fn %in% c("key", "key_data")) return(eval_tidy(key)) # key(x)
    if (call_fn != "id") {
      abort(sprintf("Please use `key = id(...)`, not `%s(...)`.", call_fn))
    } # vars(x)
  }
  key_expr <- get_expr(key)
  if (is_string(key_expr)) {
    abort(suggest_key(key_expr))
  } 
  safe_key <- purrr::safely(eval_tidy)(
    key_expr,
    env = child_env(get_env(key), id = id)
  )
  res <- safe_key$result
  if (is.data.frame(res)) {
    assert_key_data(res)
    return(res)
  } else if (is_null(safe_key$error)) {
    fn <- function(x) {
      if (is_list(x)) all(map_lgl(x, fn)) else is_expression(x)
    }
    lgl <- fn(res)
    if (lgl) return(res)
  }
  abort(suggest_key(as_string(key_expr)))
}

#' Test duplicated observations determined by key and index variables
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
#' is_duplicated(harvest, key = id(fruit), index = year)
#' are_duplicated(harvest, key = id(fruit), index = year)
#' are_duplicated(harvest, key = id(fruit), index = year, from_last = TRUE)
#' duplicates(harvest, key = id(fruit), index = year)
is_duplicated <- function(data, key = id(), index) {
  key <- use_id(data, !! enquo(key))
  index <- validate_index(data, enquo(index))

  duplicated_key_index(data, key = key, index = index)
}

#' @param from_last `TRUE` does the duplication check from the last of identical
#' elements.
#'
#' @rdname duplicates
#' @export
are_duplicated <- function(data, key = id(), index, from_last = FALSE) {
  key <- use_id(data, !! enquo(key))
  index <- validate_index(data, enquo(index))

  res <- 
    mutate(
      grouped_df(data, vars = key), 
      !! "zzz" := duplicated.default(!! index, fromLast = from_last)
    )
  res$zzz
}

#' @rdname duplicates
#' @export
duplicates <- function(data, key = id(), index) {
  key <- use_id(data, !! enquo(key))
  index <- validate_index(data, enquo(index))

  ungroup(
    filter(
      grouped_df(data, vars = key),
      duplicated.default(!! index) |
      duplicated.default(!! index, fromLast = TRUE)
    )
  )
}

remove_tsibble_attrs <- function(x) {
  attr(x, "key") <- attr(x, "index") <- attr(x, "index2") <- NULL
  attr(x, "interval") <- attr(x, "regular") <- attr(x, "ordered") <- NULL
  x
}

assert_key_data <- function(x) {
  if(is_false(
    is.data.frame(x) &&
    NCOL(x) > 0 &&
    is.list(x[[NCOL(x)]]) &&
    tail(names(x), 1L) == ".rows"
  )) {
    abort("The `key` attribute must be a data frame with its last column called `.rows`.")
  }
}
