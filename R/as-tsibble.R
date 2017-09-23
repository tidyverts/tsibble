globalVariables(c("key", "value", "zzz"))

#' Coerce to a tsibble object
#'
#' @param x Other objects to be coerced to tsibble.
#' @param ... Key variables.
#' @param index A bare (or unquoted) variable indicating time index.
#' @param validate Logical.
#'
#' @return A tsibble object.
#' @rdname as-tsibble
#'
#' @examples
#' # coerce data.frame to tsibble
#' df <- data.frame(
#'   date = rep(seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1), 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30)
#' )
#' as_tsibble(df, group) # "date" is automatically considered as the index var
#' as_tsibble(df, group, index = date) # specify the index var
#'
#' @export
as_tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

#' @rdname as-tsibble
#' @export
as_tsibble.tbl_df <- function(x, ..., index, validate = TRUE) {
  index <- enquo(index)
  tsibble_tbl(x, ..., index = index, validate = validate)
}

#' @rdname as-tsibble
#' @export
as_tsibble.data.frame <- as_tsibble.tbl_df

#' @rdname as-tsibble
#' @usage NULL
#' @export
as_tsibble.tbl <- as_tsibble.tbl_df

#' @rdname as-tsibble
#' @export
as_tsibble.list <- as_tsibble.tbl_df

#' @rdname as-tsibble
#' @usage NULL
#' @export
as_tsibble.grouped_df <- function(x, ..., index, validate = TRUE) {
  x <- dplyr::ungroup(x)
  index <- enquo(index)
  tsibble_tbl(x, ..., index = index, validate = validate)
}

#' @rdname as-tsibble
#' @export
as_tsibble.default <- function(x, ...) {
  abort("as_tsibble doesn't know how to deal with this type of class yet.")
}

#' Helper functions
#'
#' @param x A tsibble object.
#'
#' @rdname helper
#' @export
key <- function(x) {
  if (is_false(is_tsibble(x))) {
    abort(paste(expr_label(substitute(x)), "is not a tsibble."))
  }
  attr(x, "key")
}

#' @rdname helper
#' @export
interval <- function(x) {
  if (is_false(is_tsibble(x))) {
    abort(paste(expr_label(substitute(x)), "is not a tsibble."))
  }
  attr(x, "interval")
}

#' @rdname helper
#' @export
index <- function(x) {
  if (is_false(is_tsibble(x))) {
    abort(paste(expr_label(substitute(x)), "is not a tsibble."))
  }
  attr(x, "index")
}

#' Test if the object is a tsibble
#'
#' @param x An object.
#'
#' @return TRUE if the object inherits from the tbl_ts class.
#' @rdname is-tsibble
#' @export
is_tsibble <- function(x) {
  inherits(x, "tbl_ts")
}

#' @rdname is-tsibble
#' @export
is.tsibble <- is_tsibble

#' @rdname as-tsibble
#' @export
#' @usage NULL
as.tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

## tsibble is a special class of tibble that handles temporal data. It
## requires a sequence of time index to be unique across every identifier.
tsibble_tbl <- function(x, ..., index, validate = TRUE) {
  tbl <- tibble::as_tibble(x) # x is lst, data.frame, tbl_df
  cls_tbl <- c("tbl_df", "tbl", "data.frame") # basic classes

  # extract or pass the index var
  index <- extract_index_var(tbl, index = index)
  # validate key vars
  key_vars <- validate_key(data = tbl, ...)
  key_lens <- length(key_vars)
  cls_tbl <- if (key_lens > 1) {
    c("tbl_gts", "tbl_ts", cls_tbl)
  } else {
    c("tbl_ts", cls_tbl)
  }
  # validate tbl_ts
  if (validate) {
    tbl <- validate_tbl_ts(data = tbl, key = key_vars, index = index)
  }
  eval_idx <- eval_tidy(index, data = tbl)
  tbl_interval <- pull_interval(eval_idx, exclude_zero = FALSE)

  attr(tbl, "key") <- structure(key_vars, class = "key")
  attr(tbl, "index") <- structure(index, class = "index")
  attr(tbl, "interval") <- structure(tbl_interval, class = "interval")
  structure(tbl, class = cls_tbl)
}

support_type <- function() {
  c("time", "dttm", "date", "yrmon", "yrqtr", "int", "dbl")
}

detect_type <- function() {
  c("time", "dttm", "date", "yrmon", "yrqtr")
}

## Although the "index" arg is possible to automate the detection of time
## objects, it would fail when tsibble contain multiple time objects.
extract_index_var <- function(data, index) {
  cols_type <- purrr::map_chr(data, tibble::type_sum)
  if (quo_is_missing(index)) {
    val_idx <- cols_type %in% detect_type()
    if (sum(val_idx) != 1) {
      abort("Please specify the 'index' varible.")
    }
    chr_index <- colnames(data)[val_idx]
    inform(paste("The 'index' variable:", chr_index))
    sym_index <- sym(chr_index)
    index <- as_quosure(sym_index)
  } else {
    idx_type <- cols_type[quo_text(index, width = 500L)]
    if (is_false(any(idx_type %in% support_type()))) {
      abort(paste(idx_type, "is invalid for tbl_ts."))
    }
  }
  index
}

# check if a comb of key vars result in a unique data entry
# if TRUE return evaluated time index, otherwise raise an error
validate_tbl_ts <- function(data, key, index) {
  comb_key <- reduce_key(key)
  tbl_dup <- data %>%
    dplyr::group_by(!!! comb_key) %>%
    dplyr::summarise(zzz = anyDuplicated(!! index))
  if (any_c(tbl_dup$zzz, 0)) {
      abort("The 'index' variable must contain unique time stamp for each combination of key variables.")
  }
  data
}
