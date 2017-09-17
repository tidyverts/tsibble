as_tsibble <- function(x, ...) {
  UseMethod("as_tsibble")
}

as_tsibble.tbl_df <- function(x, index, ...) {
  index <- enquo(index)
  tsibble_tbl(x, index = index, ...)
}

as.tsibble.data.frame <- as_tsibble.tbl_df

as.tsibble.tbl <- as_tsibble.tbl_df

as_tsibble.list <- function(x, index, ...) {
  index <- enquo(index)
  tsibble_tbl(x, index = index, ...)
}

key <- function(tbl_ts) {
  attr(tbl_ts, "key")
}

interval <- function(tbl_ts) {
  attr(tbl_ts, "interval")
}

index <- function(tbl_ts) {
  attr(tbl_ts, "index")
}

tsibble_tbl <- function(x, index, ...) {
  tbl <- tibble::as_tibble(x) # x is lst, data.frame, tbl_df
  cls_tbl <- c("tbl_df", "tbl", "data.frame") # basic classes

  # extract or pass the index var
  index_var <- extract_index_var(tbl, index = index)
  # validate key vars
  key_vars <- validate_key(data = x, ...)
  key_lens <- length(key_vars)
  cls_tbl <- if (key_lens > 1) {
    c("tbl_gts", "tbl_ts", cls_tbl)
  } else {
    c("tbl_ts", cls_tbl)
  }
  # validate tbl_ts
  eval_lst_idx <- validate_tbl_ts(data = tbl, index = index, key = key_vars)

  tbl_interval <- extract_interval(eval_lst_idx)

  attr(tbl, "key") <- key_vars
  attr(tbl, "index") <- index
  attr(tbl, "interval") <- tbl_interval
  structure(tbl, class = cls_tbl)
}

support_type <- function() {
  c("time", "dttm", "date", "yrmon", "yrqtr", "int", "dbl")
}

detect_type <- function() {
  c("time", "dttm", "date", "yrmon", "yrqtr")
}

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

extract_interval <- function(lst_idx) {
  vec_interval <- vapply(lst_idx, function(x) gen_interval(x), numeric(1))
  idx_interval <- which.min(vec_interval)
  pull_interval(lst_idx[[idx_interval]])
}
