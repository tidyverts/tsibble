#' @importFrom tibble type_sum
#' @export
type_sum.tbl_ts <- function(x) {
  "tsibble"
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.tbl_ts <- function(x) {
  fnt_int <- format(interval(x))
  idx <- x[[index_var(x)]]
  if (has_tz(idx)) {
    tz <- angle_brackets(format_tz(idx))
    first <- c("A tsibble" = paste(dim_tbl_ts(x), brackets(fnt_int), tz))
  } else {
    first <- c("A tsibble" = paste(dim_tbl_ts(x), brackets(fnt_int)))
  }
  if (is_empty(key(x))) {
    first
  } else {
    n_keys <- big_mark(n_keys(x))
    key_sum <- c("Key" = paste(comma(key_vars(x)), brackets(n_keys)))
    c(first, key_sum)
  }
}

#' @export
tbl_sum.grouped_ts <- function(x) {
  n_grps <- big_mark(length(group_rows(x)))
  if (n_grps == 0) {
    n_grps <- "?"
  }
  grps <- group_vars(x)
  idx2 <- quo_name(index2(x))
  grp_var <- setdiff(grps, idx2)
  idx_suffix <- paste("@", idx2)
  res_grps <- NextMethod()
  res <- res_grps[head(names(res_grps), -1L)] # rm "Groups"
  n_grps <- brackets(n_grps)
  if (is_empty(grp_var)) {
    c(res, "Groups" = paste(idx_suffix, n_grps))
  } else if (has_length(grps, length(grp_var))) {
    c(res, "Groups" = paste(comma(grp_var), n_grps))
  } else {
    c(res, "Groups" = paste(comma(grp_var), idx_suffix, n_grps))
  }
}
