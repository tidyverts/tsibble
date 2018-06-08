#' Arrange rows by variables
#'
#' @param .data A `tbl_ts`.
#' @param ...  A set of unquoted variables, separated by commas.
#' @param .by_group `TRUE` will sort first by grouping variables.
#'
#' @details If not arranging key and index in ascending order, a warning is
#' likely to be issued.
#' @rdname arrange
#' @seealso [dplyr::arrange]
#' @export
arrange.tbl_ts <- function(.data, ...) {
  quos <- enquos(...)
  if (is_empty(quos)) {
    return(.data)
  }
  ordered <- ordered_by_arrange(.data, !!! quos)

  arr_data <- NextMethod()
  update_tsibble(arr_data, .data, ordered = ordered, interval = interval(.data))
}

#' @rdname arrange
#' @seealso [dplyr::arrange]
#' @export
arrange.grouped_ts <- function(.data, ..., .by_group = FALSE) {
  quos <- enquos(...)
  if (is_empty(quos)) {
    return(.data)
  }
  ordered <- ordered_by_arrange(.data, !!! quos, .by_group = .by_group)

  arr_data <- arrange(as_tibble(.data), !!! quos, .by_group = .by_group)
  update_tsibble(arr_data, .data, ordered = ordered, interval = interval(.data))
}

ordered_by_arrange <- function(.data, ..., .by_group = FALSE) {
  vars <- quos <- enquos(...)
  if (.by_group) {
    grps <- quos(!!! syms(key_flatten(groups(.data))))
    vars <- quos <- c(grps, vars)
  }
  call_pos <- purrr::map_lgl(quos, quo_is_call)
  vars[call_pos] <- first_arg(vars[call_pos])
  val_vars <- validate_vars(vars, names(.data))
  idx <- quo_text(index(.data))
  idx_pos <- val_vars %in% idx
  idx_is_call <- dplyr::first(quos[idx_pos])
  key <- key(.data)
  red_key <- as.character(key_distinct(key))
  if (is_false(any(idx_pos))) { # no index presented in the ...
    mvars <- measured_vars(.data)
    # if there's any measured variable in the ..., the time order will change.
    ordered <- ifelse(any(mvars %in% val_vars), FALSE, TRUE)
  } else if (quo_is_call(idx_is_call)) { # desc(index)
    fn <- call_name(idx_is_call)
    ordered <- ifelse(fn == "desc", FALSE, TRUE)
  } else {
    exp_vars <- c(red_key, idx)
    exp_idx <- which(val_vars %in% exp_vars)
    nested <- any(is_nest(key))
    if (n_keys(.data) < 2) { # univariate, index present
      ordered <- TRUE
    } else if (is_false(nested) &&
      all(exp_idx == seq_along(exp_idx)) &&
      is_false(idx_pos[1])
    ) {
      ordered <- TRUE
    } else if (is_true(nested) &&
      has_length(exp_idx, length(exp_vars)) &&
      is_false(idx_pos[1])) {
      ordered <- TRUE
    } else {
      ordered <- FALSE
    }
  }
  ordered
}

#' Return rows with matching conditions
#'
#' @param .data A `tbl_ts`.
#' @param ...  Logical predicates defined in terms of the variables.
#' @seealso [dplyr::filter]
#' @export
filter.tbl_ts <- function(.data, ...) {
  by_row(filter, .data, ordered = is_ordered(.data), interval = NULL, ...)
}

#' Selects rows by position
#'
#' @param .data A `tbl_ts`.
#' @param ...  Unique integers of row numbers to be selected.
#' @details If row numbers are not in ascending order, a warning is likely to
#' be issued.
#' @seealso [dplyr::slice]
#' @export
slice.tbl_ts <- function(.data, ...) {
  pos <- enquos(...)
  if (length(pos) > 1) {
    abort("`slice()` only accepts one expression.")
  }
  pos_eval <- eval_tidy(expr(!! dplyr::first(pos)))
  ascending <- row_validate(pos_eval)
  by_row(slice, .data, ordered = ascending, interval = NULL, ...)
}

row_validate <- function(x) {
  pos_dup <- anyDuplicated.default(x)
  if (any_not_equal_to_c(pos_dup, 0)) {
    abort(sprintf("Duplicated integers occurs to the position of %i.", pos_dup))
  }
  is_ascending(x)
}

#' Select/rename variables by name
#'
#' @param .data A tsibble.
#' @param ... Unquoted variable names separated by commas. `rename()` requires
#' named arguments.
#' @param .drop `FALSE` returns a tsibble object as the input. `TRUE` drops a
#' tsibble and returns a tibble.
#'
#' @details
#' These column-wise verbs from dplyr have an additional argument of `.drop = FALSE`
#' for tsibble. The index variable cannot be dropped for a tsibble. If any key
#' variable is changed, it will validate whether it's a tsibble internally.
#' Turning `.drop = TRUE` converts to a tibble first and then do the operations.
#' @rdname select
#' @seealso [dplyr::select]
#' @export
select.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) {
    return(select(as_tibble(.data), ...))
  }
  tsibble_select(.data, ...)
}

#' @rdname select
#' @seealso [dplyr::rename]
#' @export
rename.tbl_ts <- function(.data, ...) {
  tsibble_rename(.data, ...)
}

#' Add new variables
#'
#' `mutate()` adds new variables; `transmute()` keeps the newly created variables
#' along with index and keys;
#'
#' @inheritParams select.tbl_ts
#' @param ... Name-value pairs of expressions.
#'
#' @details
#' These column-wise verbs from dplyr have an additional argument of `.drop = FALSE`
#' for tsibble. The index variable cannot be dropped for a tsibble. If any key
#' variable is changed, it will validate whether it's a tsibble internally.
#' Turning `.drop = TRUE` converts to a tibble first and then do the operations.
#' * `summarise()` will not collapse on the index variable.
#' @rdname mutate
#' @seealso [dplyr::mutate]
#' @export
mutate.tbl_ts <- function(.data, ..., .drop = FALSE) {
  mut_data <- mutate(as_tibble(.data), ...)
  if (.drop) {
    return(mut_data)
  }
  lst_quos <- enquos(..., .named = TRUE)
  vec_names <- union(names(lst_quos), names(.data))
  # either key or index is present in ...
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index(vec_names, .data)
  val_key <- has_any_key(vec_names, .data)
  validate <- val_idx || val_key
  build_tsibble(
    mut_data, key = key(.data), index = !! index(.data), 
    index2 = !! index2(.data), groups = groups(.data), 
    regular = is_regular(.data), validate = validate, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

#' @rdname mutate
#' @seealso [dplyr::transmute]
#' @export
transmute.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) {
    return(transmute(as_tibble(.data), ...))
  }
  lst_quos <- enquos(..., .named = TRUE)
  mut_data <- mutate(.data, !!! lst_quos)
  idx_key <- c(quo_text(index(.data)), key_flatten(key(.data)))
  vec_names <- union(idx_key, names(lst_quos))
  select(mut_data, vec_names)
}

#' Collapse multiple rows to a single value
#'
#' @inheritParams mutate.tbl_ts
#'
#' @details Time index will not be collapsed by `summarise.tbl_ts`.
#' @rdname summarise
#' @seealso [dplyr::summarise]
#' @examples
#' # Sum over sensors ----
#' pedestrian %>%
#'   summarise(Total = sum(Count))
#' # Sum over sensors by days ----
#' pedestrian %>%
#'   index_by(Date) %>% 
#'   summarise(Total = sum(Count))
#' ## .drop = TRUE ----
#' pedestrian %>%
#'   summarise(Total = sum(Count), .drop = TRUE)
#' @export
summarise.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) {
    return(summarise(as_tibble(.data), ...))
  }
  idx <- index(.data)
  idx2 <- index2(.data)

  lst_quos <- enquos(..., .named = TRUE)
  idx2_chr <- quo_name(idx2)
  nonkey <- setdiff(names(lst_quos), 
    squash(c(key(.data), quo_name(idx), idx2_chr))
  )
  nonkey_quos <- lst_quos[nonkey]

  sum_data <- as_tibble2(.data) %>% 
    summarise(!!! nonkey_quos)
  if (identical(idx, idx2)) {
    int <- interval(.data)
    reg <- is_regular(.data)
  } else {
    int <- NULL
    reg <- TRUE
  }
  grps <- group_vars(.data)
  new_key <- key(key_reduce(.data, grps, validate = FALSE))

  build_tsibble(
    sum_data, key = new_key, index = !! idx2,
    groups = grp_drop(grps, idx2_chr), validate = FALSE, 
    regular = reg, ordered = TRUE, interval = int
  )
}

#' @rdname summarise
#' @seealso [dplyr::summarize]
#' @export
summarize.tbl_ts <- summarise.tbl_ts

#' Group by one or more variables
#'
#' @param .data A tsibble.
#' @inheritParams dplyr::group_by
#' @param x A (grouped) tsibble.
#'
#' @rdname group-by
#' @seealso [dplyr::group_by]
#' @importFrom dplyr grouped_df
#' @export
#' @examples
#' data(tourism)
#' tourism %>%
#'   group_by(Region, State) %>%
#'   summarise(geo_trips = sum(Trips))
group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  grped_tbl <- group_by(as_tibble(.data), ..., add = add)

  build_tsibble(
    grped_tbl, key = key(.data), index = !! index(.data), 
    index2 = !! index2(.data), groups = groups(grped_tbl), validate = FALSE, 
    regular = is_regular(.data), ordered = is_ordered(.data), 
    interval = interval(.data)
  )
}

#' @rdname group-by
#' @seealso [dplyr::ungroup]
#' @export
ungroup.grouped_ts <- function(x, ...) {
  build_tsibble(
    x, key = key(x), index = !! index(x), groups = id(),
    validate = FALSE, regular = is_regular(x), ordered = is_ordered(x),
    interval = interval(x)
  )
}

#' @seealso [dplyr::ungroup]
#' @export
ungroup.tbl_ts <- function(x, ...) {
  attr(x, "index2") <- index(x)
  x
}

#' @export
distinct.tbl_ts <- function(.data, ..., .keep_all = FALSE) {
  distinct(as_tibble(.data), ...)
}

by_row <- function(FUN, .data, ordered = TRUE, interval = NULL, ...) {
  FUN <- match.fun(FUN, descend = FALSE)
  tbl <- FUN(as_tibble(.data), ...)
  update_tsibble(tbl, .data, ordered = ordered, interval = interval)
}

# put new data with existing attributes
update_tsibble <- function(new, old, ordered = TRUE, interval = NULL) {
  restore_index_class(build_tsibble(
    new, key = key(old), index = !! index(old), index2 = !! index2(old),
    groups = groups(old), regular = is_regular(old), validate = FALSE, 
    ordered = ordered, interval = interval
  ), old)
}

# needed when grouping by index2 (e.g. summarise)
as_tibble2 <- function(x) {
  grps <- groups(x)
  idx2 <- index2(x)
  x <- tibble::new_tibble(x)
  group_by(x, !!! flatten(c(grps, idx2)))
}
