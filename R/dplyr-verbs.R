#' Tidyverse methods for tsibble
#'
#' * `arrange()`: if not arranging key and index in past-to-future order, a warning is
#' likely to be issued.
#' * `slice()`: if row numbers are not in ascending order, a warning is likely to
#' be issued.
#' * `select()`: keeps the variables you mention as well as the index.
#' * `transmute()`: keeps the variable you operate on, as well as the index and key.
#' * `summarise()` reduces a sequence of values over time instead of a single summary.
#' * Column-wise verbs, including `select()`, `transmute()`, `summarise()`,
#' `mutate()` & `transmute()`, keep the time context hanging around. That is,
#' the index variable cannot be dropped for a tsibble. If any key variable
#' is changed, it will validate whether it's a tsibble internally. Use `as_tibble()`
#' to leave off the time context.
#' * `unnest()` requires argument `key = id()` to get back to a tsibble.
#'
#' @param .data A `tbl_ts`.
#' @param ... same arguments accepted as its dplyr generic.
#' @inheritParams dplyr::arrange
#'
#' @name tidyverse
#' @rdname tidyverse
#' @export
arrange.tbl_ts <- function(.data, ...) {
  exprs <- enquos(...)
  if (is_empty(exprs)) return(.data)
  ordered <- ordered_by_arrange(.data, !!! exprs)

  arr_data <- arrange(as_tibble(.data), !!! exprs)
  update_tsibble(arr_data, .data, ordered = ordered, interval = interval(.data))
}

#' @rdname tidyverse
#' @export
arrange.grouped_ts <- function(.data, ..., .by_group = FALSE) {
  exprs <- enquos(...)
  if (is_empty(exprs)) return(.data)
  ordered <- ordered_by_arrange(.data, !!! exprs, .by_group = .by_group)

  arr_data <- arrange(as_tibble(.data), !!! exprs, .by_group = .by_group)
  update_tsibble(arr_data, .data, ordered = ordered, interval = interval(.data))
}

ordered_by_arrange <- function(.data, ..., .by_group = FALSE) {
  vars <- exprs <- enquos(...)
  if (.by_group) {
    grps <- groups(.data)
    vars <- exprs <- c(as_quosures(grps, env = caller_env()), vars)
  }
  call_pos <- map_lgl(exprs, quo_is_call)
  vars[call_pos] <- first_arg(vars[call_pos])
  val_vars <- tidyselect::vars_select(names(.data), !!! vars)
  idx <- index_var(.data)
  idx_pos <- val_vars %in% idx
  idx_is_call <- dplyr::first(exprs[idx_pos])
  key <- key(.data)
  if (is_false(any(idx_pos))) { # no index presented in the ...
    mvars <- measured_vars(.data)
    # if there's any measured variable in the ..., the time order will change.
    !any(mvars %in% val_vars)
  } else if (quo_is_call(idx_is_call)) { # desc(index)
    fn <- call_name(idx_is_call)
    fn != "desc"
  } else {
    exp_vars <- c(key, idx)
    exp_idx <- which(val_vars %in% exp_vars)
    n_keys(.data) < 2 ||
      all(exp_idx == seq_along(exp_idx)) &&
      has_length(exp_idx, length(exp_vars)) &&
      is_false(idx_pos[1])
  }
}

#' @rdname tidyverse
#' @export
filter.tbl_ts <- function(.data, ..., .preserve = FALSE) {
  by_row(filter, .data, ordered = is_ordered(.data), interval = NULL, ...,
    .preserve = .preserve
  )
}

#' @rdname tidyverse
#' @export
slice.tbl_ts <- function(.data, ..., .preserve = FALSE) {
  pos <- enquos(...)
  if (length(pos) > 1) {
    abort("`slice()` only accepts one expression.")
  }
  pos_eval <- eval_tidy(expr(!! dplyr::first(pos)))
  ascending <- row_validate(pos_eval)
  by_row(slice, .data, ordered = ascending, interval = NULL, ..., 
    .preserve = .preserve)
}

row_validate <- function(x) {
  if (is_logical(x)) return(x)
  pos_dup <- anyDuplicated.default(x)
  if (any_not_equal_to_c(pos_dup, 0)) {
    abort(sprintf("Duplicated integers occur to the position of %i.", pos_dup))
  }
  is_ascending(x)
}

#' @param .drop Defunct, please use `as_tibble()` for `.drop = TRUE` instead.
#' `FALSE` returns a tsibble object as the input. `TRUE` drops a tsibble and
#' returns a tibble.
#'
#' @rdname tidyverse
#' @export
select.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) abort_drop()

  lst_quos <- enquos(...)
  lst_exprs <- map(lst_quos, quo_get_expr)
  idx_chr <- index_var(.data)
  rm_index <- sym(paste0("-", idx_chr))
  if (any(map_lgl(lst_exprs, function(x) x == rm_index))) {
    abort(sprintf(
      "Column `%s` (index) can't be removed.\nDo you need `as_tibble()` to work with data frame?",
      idx_chr
    ))
  }

  named <- list_is_named(lst_quos)
  .data <- rename_tsibble(.data, !!! lst_quos[named])

  lst_quos[named] <- names(lst_quos)[named]
  select_tsibble(.data, !!! lst_quos)
}

#' @rdname tidyverse
#' @export
rename.tbl_ts <- function(.data, ...) {
  rename_tsibble(.data, ...)
}

#' @rdname tidyverse
#' @export
mutate.tbl_ts <- function(.data, ..., .drop = FALSE) {
  mut_data <- mutate(as_tibble(.data), ...)
  if (.drop) abort_drop()

  idx_chr <- index_var(.data)
  if (is_false(idx_chr %in% names(mut_data))) { # index has been removed
    abort(sprintf(
      "Column `%s` (index) can't be removed.\nDo you need `as_tibble()` to work with data frame?",
      idx_chr
    ))
  }

  lst_quos <- enquos(..., .named = TRUE)
  vec_names <- names(lst_quos)
  # either key or index is present in ...
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index(vec_names, .data)
  val_key <- has_any_key(vec_names, .data)
  if (val_key) {
    key_vars <- setdiff(names(mut_data), measured_vars(.data))
    .data <- remove_key(.data, key_vars)
  }
  validate <- val_idx || val_key
  if (validate) {
    mut_data <- retain_tsibble(mut_data, key(.data), index(.data))
  }
  build_tsibble(
    mut_data, key = key_data(.data), index = !! index(.data),
    index2 = !! index2(.data), regular = is_regular(.data),
    ordered = is_ordered(.data), interval = interval(.data),
    validate = FALSE
  )
}

#' @rdname tidyverse
#' @export
transmute.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) abort_drop()

  lst_quos <- enquos(..., .named = TRUE)
  mut_data <- mutate(.data, !!! lst_quos)
  idx_key <- c(index_var(.data), key_vars(.data))
  vec_names <- union(idx_key, names(lst_quos))
  select(mut_data, !!! vec_names)
}

#' @rdname tidyverse
#' @export
#' @examples
#' # Sum over sensors ----
#' pedestrian %>%
#'   summarise(Total = sum(Count))
#' # Back to tibble
#' pedestrian %>%
#'   as_tibble() %>%
#'   summarise(Total = sum(Count))
summarise.tbl_ts <- function(.data, ..., .drop = FALSE) {
  # Unlike summarise.grouped_df(), summarise.tbl_ts() doesn't compute values for 
  # empty groups. Bc the index is an implicit grouping data, empty groups have
  # been dropped.
  if (.drop) abort_drop()

  idx <- index(.data)
  idx2 <- index2(.data)

  lst_quos <- enquos(..., .named = TRUE)
  idx2_chr <- as_string(idx2)
  nonkey <- setdiff(
    names(lst_quos),
    squash(c(key(.data), as_string(idx), idx2_chr))
  )
  nonkey_quos <- lst_quos[nonkey]

  grped_data <- group_by_index2(.data)
  grps <- groups(grped_data)
  len_grps <- length(grps)
  sum_data <- grped_data %>%
    summarise(!!! nonkey_quos) %>%
    group_by(!!! grps[-((len_grps - 1):len_grps)]) # remove index2 and last grp
  if (identical(idx, idx2)) {
    int <- interval(.data)
    reg <- is_regular(.data)
  } else {
    int <- NULL
    reg <- TRUE
  }
  grps <- syms(setdiff(group_vars(.data), as_string(idx2)))

  build_tsibble(
    sum_data, key = grps, index = !! idx2, regular = reg, ordered = TRUE, 
    interval = int, validate = FALSE
  )
}

#' @rdname tidyverse
#' @export
summarize.tbl_ts <- summarise.tbl_ts

#' @inheritParams dplyr::group_by
#' @importFrom dplyr grouped_df
#' @rdname tidyverse
#' @export
group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  grped_tbl <- group_by(as_tibble(.data), ..., add = add)
  build_tsibble_meta(
    grped_tbl, key = key_data(.data), index = !! index(.data),
    index2 = !! index2(.data), regular = is_regular(.data),
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

#' Group by key variables
#'
#' @param .data A `tbl_ts` object.
#' @param ... Ignored.
#' @export
#' @examples
#' tourism %>%
#'   group_by_key()
group_by_key <- function(.data, ...) {
  dplyr::group_by(.data, !!! key(.data))
}

#' @rdname tidyverse
#' @export
ungroup.grouped_ts <- function(x, ...) {
  tbl <- ungroup(as_tibble(x))
  build_tsibble_meta(
    tbl, key = key_data(x), index = !! index(x), regular = is_regular(x),
    ordered = is_ordered(x), interval = interval(x)
  )
}

#' @export
ungroup.tbl_ts <- function(x, ...) {
  attr(x, "index2") <- index(x)
  x
}

#' @export
distinct.tbl_ts <- function(.data, ..., .keep_all = FALSE) {
  distinct(as_tibble(.data), ...)
}

abort_drop <- function() {
  abort("Argument `.drop` is defunct. Please use `as_tibble()` instead.")
}
