globalVariables(c("key", "value"))

#' Coerce to a tsibble object
#'
#' @param x Other objects to be coerced to tsibble.
#' @param index A bare (or unquoted) variable indicating time index
#' @param ... Key Bare variables.
#'
#' @return A tsibble object.
#' @author Earo Wang
#' @seealso [tibble::as_tibble]
#' @rdname as-tsibble
#'
#' @examples
#'    # coerce data.frame to tsibble
#'    # as_tsibble(tidypkgs, index = date, package) 
#'
#'    # coerce ts to tsibble
#'    # as_tsibble(AirPassengers)
#'    # as_tsibble(sunspot.year)
#'    # as_tsibble(sunspot.month)
#'    # as_tsibble(austres)
#'
#' @export
as_tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

#' @rdname as-tsibble
#' @export
as_tsibble.tbl_df <- function(x, index, ...) {
  index <- enquo(index)
  tsibble_tbl(x, index = index, ...)
}

#' @rdname as-tsibble
#' @export
as_tsibble.data.frame <- as_tsibble.tbl_df

#' @rdname as-tsibble
#' @export
as_tsibble.tbl <- as_tsibble.tbl_df

#' @rdname as-tsibble
#' @export
as_tsibble.list <- as_tsibble.tbl_df

#' @rdname as-tsibble
#' @export
as_tsibble.grouped_df <- function(x, index, ...) {
  x <- dplyr::ungroup(x)
  index <- enquo(index)
  tsibble_tbl(x, index = index, ...)
}

#' @rdname as-tsibble
#' @export
as_tsibble.default <- function(x, index, ...) {
  abort("as_tsibble doesn't know how to deal with this type of class yet.")
}

#' @rdname as-tsibble
#' @export
key <- function(x) {
  attr(x, "key")
}

#' @rdname as-tsibble
#' @export
interval <- function(x) {
  attr(x, "interval")
}

#' @rdname as-tsibble
#' @export
index <- function(x) {
  attr(x, "index")
}

## tsibble is a special class of tibble that handles temporal data. It
## requires a sequence of time index to be unique across every identifier.
tsibble_tbl <- function(x, index, ..., validate = TRUE) {
  tbl <- tibble::as_tibble(x) # x is lst, data.frame, tbl_df
  cls_tbl <- c("tbl_df", "tbl", "data.frame") # basic classes

  # extract or pass the index var
  index_var <- extract_index_var(tbl, index = index)
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
    eval_lst_idx <- validate_tbl_ts(data = tbl, index = index, key = key_vars)
    tbl_interval <- extract_interval(eval_lst_idx)
  }

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
    inform(paste("The 'index' variable is", chr_index))
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
validate_tbl_ts <- function(data, index, key) {
  comb_key <- reduce_key(key)
  tbl_nest <- nest_data(data, !!! comb_key)
  eval_lst_idx <- purrr::map(tbl_nest$data, ~ eval_tidy(index, data = .)) 
  unique_idx <- purrr::map_int(eval_lst_idx, anyDuplicated)
  if (any(unique_idx != 0)) {
      abort("The 'index' variable must contain unique time stamp for each combination of key variables.")
  }
  eval_lst_idx
}

# pull_interval takes a vector of evaluated time index,
# extract_interval deals with a list of time index instead.
extract_interval <- function(lst_idx) {
  vec_interval <- vapply(lst_idx, function(x) gen_interval(x), numeric(1))
  idx_interval <- which.min(vec_interval)
  pull_interval(lst_idx[[idx_interval]])
}

nest_data <- function(data, ...) {
  data %>% 
    dplyr::group_by(!!! quos(...)) %>% 
    tidyr::nest()
}
