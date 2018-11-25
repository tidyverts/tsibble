#' @export
type_sum.tbl_ts <- function(x) {
  "tsibble"
}

#' @export
type_sum.yearweek <- function(x) {
  "week"
}

#' @export
type_sum.yearmonth <- function(x) {
  "mth"
}

#' @export
type_sum.yearquarter <- function(x) {
  "qtr"
}

#' @export
tbl_sum.tbl_ts <- function(x) {
  int_x <- interval(x)
  fnt_int <- format(int_x)
  idx <- x[[index_var(x)]]
  if (has_tz(idx)) {
    tz <- surround(format_tz(idx), "<")
    first <- c("A tsibble" = paste(dim_tbl_ts(x), surround(fnt_int, "["), tz))
  } else {
    first <- c("A tsibble" = paste(dim_tbl_ts(x), surround(fnt_int, "[")))
  }
  if (is_empty(key(x))) {
    first
  } else {
    n_keys <- big_mark(n_keys(x))
    key_sum <- c("Key" = paste(paste_comma(key_vars(x)), surround(n_keys, "[")))
    c(first, key_sum)
  }
}

#' @export
tbl_sum.grouped_ts <- function(x) {
  n_grps <- big_mark(n_groups(x))
  if (n_grps == 0) {
    n_grps <- "?"
  }
  grps <- group_vars(x)
  idx2 <- quo_name(index2(x))
  grp_var <- setdiff(grps, idx2)
  idx_suffix <- paste("@", idx2)
  res <- NextMethod()
  n_grps <- surround(n_grps, "[")
  if (is_empty(grp_var)) {
    c(res, "Groups" = paste(idx_suffix, n_grps))
  } else if (has_length(grps, length(grp_var))) {
    c(res, "Groups" = paste(paste_comma(grp_var), n_grps))
  } else {
    c(res, "Groups" = paste(paste_comma(grp_var), idx_suffix, n_grps))
  }
}
