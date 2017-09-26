#' @seealso [dplyr::filter]
# ToDo: filter(pkgs_ts, ~ year() == 2016)? => tbl_ts
# ToDo: filter(pkgs_ts, ~ month() == 1)? => tbl_df
filter.tbl_ts <- function(.data, ...) {
  key <- key(.data)
  index <- index(.data)
  interval <- interval(.data)
  cls <- class(.data)
  .data <- NextMethod()
  return(structure(
    .data, key = key, index = index, interval = interval, class = cls
  ))
}

#' @seealso [dplyr::select]
# ToDo: select should work with everything(), ends_with() and etc. too
select.tbl_ts <- function(.data, ...) {
  cls <- class(.data)
  key <- key(.data)
  index <- index(.data)
  interval <- interval(.data)
  .data <- NextMethod()
  dots_cap <- quos(...)
  idx_there <- any(purrr::map_lgl(dots_cap, function(x) x == index))
  key_there <- any(rlang::flatten_lgl(purrr::map(key, function(x)
    purrr::map_lgl(dots_cap, function(y) y == x)
  )))
  if (idx_there && key_there) {
    return(structure(
      .data, key = key, index = index, interval = interval, class = cls
    ))
  } else {
    return(structure(.data, class = c("tbl_df", "tbl", "data.frame")))
  }
}

#' @seealso [dplyr::mutate]
#' @export
mutate.tbl_ts <- function(.data, ...) {
  mut_data <- NextMethod()
  as_tsibble(
    mut_data, !!! key(.data), index = !! f_rhs(index(.data)),
    validate = FALSE, regular = is_regular(.data)
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
