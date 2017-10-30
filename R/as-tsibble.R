globalVariables(c("key", "value", "zzz"))

#' Coerce to a tsibble object
#'
#' @param x Other objects to be coerced to a tsibble (`tbl_ts`).
#' @param ... Unquoted key variable(s) that define unique time indices. Only used
#' for data of class `tbl_df`, `data.frame` and `list` (i.e. long data form), if
#' a univariate time series (without an explicit key), simply leave it blank. 
#' Ignored for other types of classes. See below for details.
#' @param index A bare (or unquoted) variable to specify the time index variable.
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables lead to unique time indices (i.e. a valid tsibble). If you
#' are sure that it's a valid input, specify `FALSE` to skip the checks.
#' @param regular Regular time interval (`TRUE`) or irregular (`FALSE`). `TRUE`
#' finds the minimal time span as the interval.
#'
#' @inheritSection tsibble-package Index
#'
#' @inheritSection tsibble-package Key
#'
#' @details A valid tsibble does not impose the chronological order. Please use
#' [arrange] to get the order by time.
#'
#' @return A tsibble object.
#' @rdname as-tsibble
#' @aliases as.tsibble
#'
#' @examples
#' # coerce tibble to tsibble w/o a key ----
#' tbl1 <- tibble::tibble(
#'   date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
#'   value = rnorm(10)
#' )
#' # "date" is automatically considered as the index var, and "group" is the key
#' as_tsibble(tbl1)
#' # specify the index var
#' as_tsibble(tbl1, index = date)
#'
#' # coerce tibble to tsibble with one key ----
#' tbl2 <- tibble::tibble(
#'   date = rep(seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1), 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30)
#' )
#' as_tsibble(tbl2, group) 
#' as_tsibble(tbl2, group, index = date)
#'
#' @export
as_tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

#' @rdname as-tsibble
#' @export
as_tsibble.tbl_df <- function(x, ..., index, validate = TRUE, regular = TRUE) {
  index <- enquo(index)
  tsibble_tbl(
    x, key = quos(...), index = index,
    validate = validate, regular = regular
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
as_tsibble.grouped_ts <- as_tsibble.tbl_df

#' @keywords internal
#' @export
as_tsibble.grouped_df <- as_tsibble.grouped_ts

#' @keywords internal
#' @export
as_tsibble.default <- function(x, ...) {
  abort("as_tsibble doesn't know how to deal with this type of class yet.")
}

#' Return key variables from a tsibble
#'
#' `key()` returns a list of symbols; `key_vars()` gives a character vector.
#'
#' @param x A tsibble object.
#'
#' @rdname key
#'
#' @examples
#' # A single key for pedestrian data ----
#' data(pedestrian)
#' key(pedestrian)
#' key_vars(pedestrian)
#'
#' # Nested and crossed keys for tourism data ----
#' data(tourism)
#' key(tourism)
#' key_vars(tourism)
#' @export
key <- function(x) {
  if (is_false(is_tsibble(x))) {
    abort(paste(expr_label(substitute(x)), "is not a tsibble."))
  }
  attr(x, "key")
}

`key<-` <- function(x, value) {
  attr(x, "key") <- value
  x
}

#' @rdname key
#' @export
key_vars <- function(x) {
  format(key(x))
}

#' @export
groups.tbl_ts <- function(x) {
  attr(x, "vars")
}

#' @export
group_vars.tbl_ts <- function(x) {
  format(groups(x))
}

#' Return index and interval from a tsibble
#'
#' @param x A tsibble object.
#' @rdname index
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

#' @rdname index
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
#' @export
is.tsibble <- is_tsibble

#' @rdname is-tsibble
#' @usage NULL
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
tsibble_tbl <- function(x, key, index, validate = TRUE, regular = TRUE) {
  tbl <- tibble::as_tibble(x) # x is lst, data.frame, tbl_df
  cls_tbl <- c("tbl_df", "tbl", "data.frame") # basic classes

  # extract or pass the index var
  index <- extract_index_var(tbl, index = index)
  # validate key vars
  key_vars <- validate_key(data = tbl, key)
  key_lens <- length(key_vars)
  # validate tbl_ts
  if (validate) {
    tbl <- validate_tbl_ts(data = tbl, key = key_vars, index = index)
  }
  if (regular) {
    eval_idx <- eval_tidy(index, data = tbl)
    tbl_interval <- pull_interval(eval_idx, exclude_zero = FALSE)
  } else {
    tbl_interval <- list()
  }

  attr(tbl, "key") <- structure(key_vars, class = "key")
  attr(tbl, "index") <- index
  attr(tbl, "interval") <- structure(tbl_interval, class = "interval")
  attr(tbl, "regular") <- regular
  if (is_grouped_ts(x) || dplyr::is_grouped_df(x)) {
    grps <- groups(x)
    attr(tbl, "vars") <- structure(grps, class = "vars")
    cls_tbl <- c("grouped_ts", "tbl_ts", cls_tbl)
  } else {
    cls_tbl <- c("tbl_ts", cls_tbl)
  }
  structure(tbl, class = cls_tbl)
}

detect_type <- function() {
  c("time", "dttm", "date", "yrmth", "yrqtr")
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

# check if a comb of key vars result in a unique data entry
# if TRUE return the data, otherwise raise an error
validate_tbl_ts <- function(data, key, index) {
  comb_key <- reduce_key(key)
  tbl_dup <- dplyr::grouped_df(data, vars = flatten_key(comb_key)) %>%
    dplyr::summarise(zzz = anyDuplicated(!! index))
  if (any_not_equal_to_c(tbl_dup$zzz, 0)) {
    abort("Invalid tsibble: duplicated time indices")
  }
  data
}

#' @export
as_tibble.tbl_ts <- function(x, ...) {
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}

#' @export
as.tibble.tbl_ts <- as_tibble.tbl_ts

#' @export
as_tibble.grouped_ts <- function(x, ...) {
  grps <- groups(x)
  class(x) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
  attr(x, "vars") <- flatten(grps)
  x
}

#' @export
as.tibble.grouped_ts <- as_tibble.grouped_ts

#' @export
as.data.frame.tbl_ts <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- "data.frame"
  x
}
