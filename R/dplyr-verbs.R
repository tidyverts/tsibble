#' @seealso [dplyr::arrange]
#' @export
arrange.tbl_ts <- function(.data, ...) {
  arr_data <- NextMethod()
  as_tsibble(
    arr_data, !!! key(.data), index = !! f_rhs(index(.data)),
    validate = FALSE, regular = is_regular(.data)
  )
}

#' @seealso [dplyr::filter]
#' @export
filter.tbl_ts <- function(.data, ...) {
  fil_data <- NextMethod()
  as_tsibble(
    fil_data, !!! key(.data), index = !! f_rhs(index(.data)),
    validate = FALSE, regular = is_regular(.data)
  )
}

#' @seealso [dplyr::select]
#' @export
select.tbl_ts <- function(.data, ...) {
  lst_quos <- quos(..., .named = TRUE)
  val_vars <- validate_vars(j = lst_quos, x = colnames(.data))
  val_idx <- has_index_var(j = val_vars, x = .data)
  if (is_false(val_idx)) {
    abort("The index variable cannot be dropped.")
  }
  rhs <- purrr::map_chr(lst_quos, quo_name)
  lhs <- names(lst_quos)
  index(.data) <- update_index(index(.data), rhs, lhs)
  # switch to a list due to error msg from the `else` part
  sel_data <- unclass(NextMethod()) # a list
  val_key <- has_all_key(j = lst_quos, x = .data)
  if (is_true(val_key)) { # no changes in key vars
    return(as_tsibble(
      sel_data, !!! key(.data), index = !! f_rhs(index(.data)),
      validate = FALSE, regular = is_regular(.data)
    ))
  } else {
    key(.data) <- update_key(key(.data), val_vars)
    key(.data) <- update_key2(key(.data), rhs, lhs)
    as_tsibble(
      sel_data, !!! key(.data), index = !! f_rhs(index(.data)),
      validate = TRUE, regular = is_regular(.data)
    )
  }
}

#' @seealso [dplyr::rename]
#' @export
rename.tbl_ts <- function(.data, ...) {
  lst_quos <- quos(..., .named = TRUE)
  val_vars <- validate_vars(j = lst_quos, x = colnames(.data))
  if (has_index_var(val_vars, .data) || has_any_key(val_vars, .data)) {
    rhs <- purrr::map_chr(lst_quos, quo_name)
    lhs <- names(lst_quos)
    index(.data) <- update_index(index(.data), rhs, lhs)
    key(.data) <- update_key2(key(.data), rhs, lhs)
  }
  ren_data <- NextMethod()
  as_tsibble(
    ren_data, !!! key(.data), index = !! f_rhs(index(.data)),
    validate = FALSE, regular = is_regular(.data)
  )
}

#' @seealso [dplyr::mutate]
#' @export
# [!] important to check if index and key vars have be overwritten in the LHS
mutate.tbl_ts <- function(.data, ...) {
  lst_quos <- quos(..., .named = TRUE)
  vec_names <- union(names(lst_quos), colnames(.data))
  mut_data <- NextMethod()
  # either key or index is present in ...
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index_var(vec_names, .data)
  val_key <- has_any_key(vec_names, .data)
  validate <- val_idx || val_key
  as_tsibble(
    mut_data, !!! key(.data), index = !! f_rhs(index(.data)),
    validate = validate, regular = is_regular(.data)
  )
}

#' @seealso [dplyr::group_by]
#' @export
group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  index <- index(.data)
  idx_var <- format(index)
  grped_quo <- quos(...)
  grped_chr <- prepare_groups(.data, grped_quo, add = add)
  if (idx_var %in% grped_chr) {
    abort(paste("The index variable", surround(idx_var), "cannot be grouped."))
  }

  grped_ts <- grouped_ts(.data, grped_chr, grped_quo, add = add)
  as_tsibble(
    grped_ts, !!! key(.data), index = !! f_rhs(index),
    validate = FALSE, regular = is_regular(.data)
  )
}

#' @seealso [dplyr::ungroup]
#' @export
ungroup.grouped_ts <- function(x, ...) {
  x <- rm_class(x, "grouped_ts")
  groups(x) <- NULL
  as_tsibble(
    x, !!! key(x), index = !! f_rhs(index(x)),
    validate = FALSE, regular = is_regular(x)
  )
}

#' @seealso [dplyr::ungroup]
#' @export
ungroup.tbl_ts <- ungroup.grouped_ts

# this function prepares group variables in a vector of characters for
# dplyr::grouped_df()
# the arg of group can take a nested str but be flattened in the end.
prepare_groups <- function(data, group, add = FALSE) {
  if (add) {
    old_grps <- flatten_key(groups(data))
    grps <- flatten_key(validate_key(data, group))
    return(c(old_grps, grps))
  } else {
    flatten_key(validate_key(data, group))
  }
}

"groups<-" <- function(x, value) {
  attr(x, "vars") <- value
  x
}

# work around with dplyr::grouped_df (not an elegant solution)
# better to use dplyr internal cpp code when its API is stable
# the arg `vars` passed to dplyr::grouped_df
# the arg `group` takes new defined groups
grouped_ts <- function(data, vars, group, add = FALSE) { # vars are characters
  old_class <- class(data)
  grped_df <- dplyr::grouped_df(data, vars)
  class(grped_df) <- unique(c("grouped_ts", old_class))
  val_grps <- validate_key(data, group)
  if (add) {
    groups(grped_df) <- c(groups(data), val_grps)
  } else {
    groups(grped_df) <- val_grps
  }
  grped_df
}

# methods::setGeneric("groups<-")

rm_class <- function(x, value) {
  class(x) <- class(x)[-match(value, class(x))]
  x
}

replace_class <- function(x, old, new) {
  class(x)[match(old, class(x))] <- new
  x
}
