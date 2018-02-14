#' Row-wise verbs
#'
#' `arrange()` arranges rows by variable; `filter()` returns rows with matching
#' conditions; `slice()` selects rows by position.
#'
#' @param .data A tsibble.
#' @param ...
#' * A set of unquoted variables for `arrange()`.
#' * Logical predicates defined in terms of the variables for `filter()`.
#' * Integer row numbers for `slice()`.
#' @param .by_group `TRUE` will sort first by grouping variable.
#'
#' @rdname row-verb
#' @seealso [dplyr::arrange]
#' @export
arrange.tbl_ts <- function(.data, ...) {
  arr_data <- NextMethod()
  update_tsibble(arr_data, .data)
}

#' @rdname row-verb
#' @seealso [dplyr::arrange]
#' @export
arrange.grouped_ts <- function(.data, ..., .by_group = FALSE) {
  arr_data <- arrange(as_tibble(.data), ..., .by_group = .by_group)
  update_tsibble(arr_data, .data)
}

#' @rdname row-verb
#' @seealso [dplyr::filter]
#' @export
filter.tbl_ts <- function(.data, ...) {
  by_row(filter, .data, ...)
}

#' @rdname row-verb
#' @seealso [dplyr::slice]
#' @export
slice.tbl_ts <- function(.data, ...) {
  by_row(slice, .data, ...)
}

#' Column-wise verbs
#'
#' `select()` selects columns by variables; `mutate()` adds new variables;
#' `transmute()` keeps the newly created variables along with index and keys;
#' `summarise()` collapses the rows by variables.
#'
#' @param .data A tsibble.
#' @param ... A set of name-value pairs of expressions.
#' @param drop `FALSE` returns a tsibble object as the input. `TRUE` drops a
#' tsibble and returns a tibble.
#'
#' @details
#' * These column-wise verbs from dplyr have an additional argument of `drop = FALSE`
#' for tsibble. The index variable cannot be dropped for a tsibble. If any key
#' variable is changed, it will validate whether it's a tsibble internally.
#' Turning `drop = TRUE` converts to a tibble first and then do the operations.
#' * `summarise()` will not collapse on the index variable.
#' @rdname col-verb
#' @seealso [dplyr::select]
#' @export
select.tbl_ts <- function(.data, ..., drop = FALSE) {
  sel_data <- select(as_tibble(.data), ...)
  if (drop) {
    return(sel_data)
  }
  lst_quos <- quos(...)
  val_vars <- validate_vars(j = lst_quos, x = colnames(.data))
  val_idx <- has_index_var(j = val_vars, x = .data)
  if (is_false(val_idx)) {
    abort(sprintf(
      "The `index` (`%s`) must not be dropped. Do you need `drop = TRUE` to drop `tbl_ts`?", 
      quo_text2(index(.data))
    ))
  }
  lhs <- names(val_vars)
  index(.data) <- update_index(index(.data), val_vars, lhs)
  val_key <- has_all_key(j = lst_quos, x = .data)
  if (is_true(val_key)) { # no changes in key vars
    return(update_tsibble(sel_data, .data))
  }
  key(.data) <- update_key(key(.data), val_vars)
  key(.data) <- update_key2(key(.data), val_vars, lhs)
  as_tsibble(
    sel_data, key = key(.data), index = !! index(.data), 
    groups = groups(.data), regular = is_regular(.data),
    validate = TRUE,
  )
}

#' @rdname col-verb
#' @seealso [dplyr::rename]
#' @export
rename.tbl_ts <- function(.data, ...) {
  lst_quos <- quos(...)
  val_vars <- tidyselect::vars_rename(colnames(.data), !!! lst_quos)
  if (has_index_var(val_vars, .data) || has_any_key(val_vars, .data)) {
    lhs <- names(val_vars)
    index(.data) <- update_index(index(.data), val_vars, lhs)
    key(.data) <- update_key2(key(.data), val_vars, lhs)
  }
  ren_data <- NextMethod()
  update_tsibble(ren_data, .data)
}

#' @rdname col-verb
#' @seealso [dplyr::mutate]
#' @export
mutate.tbl_ts <- function(.data, ..., drop = FALSE) {
  mut_data <- mutate(as_tibble(.data), ...)
  if (drop) {
    return(mut_data)
  }
  lst_quos <- quos(..., .named = TRUE)
  vec_names <- union(names(lst_quos), colnames(.data))
  mut_data <- mutate(as_tibble(.data), ...)
  # either key or index is present in ...
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index_var(vec_names, .data)
  val_key <- has_any_key(vec_names, .data)
  validate <- val_idx || val_key
  as_tsibble(
    mut_data, key = key(.data), index = !! index(.data), 
    groups = groups(.data), regular = is_regular(.data),
    validate = validate 
  )
}

#' @rdname col-verb
#' @seealso [dplyr::transmute]
#' @export
transmute.tbl_ts <- function(.data, ..., drop = FALSE) {
  if (drop) {
    return(transmute(as_tibble(.data), ...))
  }
  lst_quos <- quos(..., .named = TRUE)
  mut_data <- mutate(.data, !!! lst_quos)
  idx_key <- c(quo_text2(index(.data)), flatten_key(key(.data)))
  vec_names <- union(idx_key, names(lst_quos))
  select(mut_data, tidyselect::one_of(vec_names))
}

#' @rdname col-verb
#' @seealso [dplyr::summarise]
#' @export
summarise.tbl_ts <- function(.data, ..., drop = FALSE) {
  if (drop) {
    return(summarise(as_tibble(.data), ...))
  }
  lst_quos <- quos(..., .named = TRUE)
  first_arg <- first_arg(lst_quos)
  vec_vars <- as.character(first_arg)
  idx <- index(.data)
  if (has_index_var(j = vec_vars, x = .data)) {
    abort(sprintf(
      "The `index` (`%s`) must not be dropped. Do you need `drop = TRUE` to drop `tbl_ts`?", 
      quo_text2(index(.data))
    ))
  }

  grps <- groups(.data)
  chr_grps <- c(quo_text2(idx), flatten_key(grps))
  sum_data <- .data %>%
    grouped_df(vars = chr_grps) %>%
    summarise(!!! lst_quos)

  as_tsibble(
    sum_data, key = grps, index = !! idx, groups = drop_group(grps),
    validate = FALSE, regular = is_regular(.data)
  )
}

#' @rdname col-verb
#' @seealso [dplyr::summarize]
#' @export
summarize.tbl_ts <- summarise.tbl_ts

#' Group by one or more variables
#'
#' @param .data A tsibble.
#' @param ... Variables to group by. It follows a consistent rule as the "key"
#' expression in [as_tsibble], which means `|` for nested variables and `,` for
#' crossed variables. The following operations will affect the tsibble structure
#' based on the way how the variables are grouped.
#' @param add `TRUE` adds to the existing groups, otherwise overwrites.
#' @param x A (grouped) tsibble.
#'
#' @rdname group-by
#' @seealso [dplyr::group_by]
#' @importFrom dplyr grouped_df
#' @export
#' @examples
#' data(tourism)
#' tourism %>%
#'   group_by(Region | State) %>%
#'   summarise(geo_trips = sum(Trips))
group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  index <- index(.data)
  idx_var <- quo_text2(index)
  current_grps <- quos(...)
  final_grps <- prepare_groups(.data, current_grps, add = add)
  grped_chr <- flatten_key(final_grps)
  if (idx_var %in% grped_chr) {
    abort(sprintf(
      "The `index` (`%s`) must not be grouped. Do you need `as_tibble()` to coerce to `tbl_df`?", 
      idx_var
    ))
  }

  grped_ts <- grouped_df(.data, grped_chr)
  as_tsibble(
    grped_ts, key = key(.data), index = !! index, groups = final_grps,
    validate = FALSE, regular = is_regular(.data)
  )
}

#' @rdname group-by
#' @seealso [dplyr::ungroup]
#' @export
ungroup.grouped_ts <- function(x, ...) {
  as_tsibble(
    x, key = key(x), index = !! index(x), groups = id(),
    validate = FALSE, regular = is_regular(x)
  )
}

#' @seealso [dplyr::ungroup]
#' @export
ungroup.tbl_ts <- function(x, ...) {
  x
}

# this function prepares group variables in a vector of characters for
# dplyr::grouped_df()
prepare_groups <- function(data, group, add = FALSE) {
  grps <- validate_key(data, group)
  if (is_false(add)) {
    return(grps)
  }
  c(groups(data), grps)
}

do.tbl_ts <- function(.data, ...) {
  dplyr::do(as_tibble(.data), ...)
}

#' @export
distinct.tbl_ts <- function(.data, ...) {
  abort("'tbl_ts' has no support for distinct(). Please coerce to 'tbl_df' first and then distinct().")
}

by_row <- function(FUN, .data, ...) {
  FUN <- match.fun(FUN, descend = FALSE)
  tbl <- FUN(as_tibble(.data), ...)
  update_tsibble(tbl, .data)
}

# put new data with existing attributes
update_tsibble <- function(new, old) {
  as_tsibble(
    new, key = key(old), index = !! index(old), groups = groups(old), 
    regular = is_regular(old), validate = FALSE
  )
}
