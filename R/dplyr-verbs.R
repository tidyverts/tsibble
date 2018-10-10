#' Tidyverse methods for tsibble
#'
#' * `arrange()`: if not arranging key and index in past-to-future order, a warning is
#' likely to be issued.
#' * `slice()`: if row numbers are not in ascending order, a warning is likely to
#' be issued.
#' * `select()`: keeps the variables you mention as well as the index. 
#' * `transmute()`: keeps the variable you operate on, as well as the index and key.
#' * `summarise()` will not collapse on the index variable.
#' * The column-wise verbs, including `select()`, `transmute()`, `summarise()`, 
#' `mutate()` & `transmute()`, have an additional argument of `.drop = FALSE` for 
#' tsibble. The index variable cannot be dropped for a tsibble. If any key variable 
#' is changed, it will validate whether it's a tsibble internally. Turning 
#' `.drop = TRUE` converts to a tibble first and then do the operations.
#'
#' @param .data A `tbl_ts`.
#' @param ... same arguments accepted as its dplyr generic.
#' @inheritParams dplyr::arrange
#'
#' @name tidyverse
#' @rdname tidyverse
#' @export
arrange.tbl_ts <- function(.data, ...) {
  quos <- enquos(...)
  if (is_empty(quos)) return(.data)
  ordered <- ordered_by_arrange(.data, !!! quos)

  arr_data <- NextMethod()
  update_tsibble(arr_data, .data, ordered = ordered, interval = interval(.data))
}

#' @rdname tidyverse
#' @export
arrange.grouped_ts <- function(.data, ..., .by_group = FALSE) {
  quos <- enquos(...)
  if (is_empty(quos)) return(.data)
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
  idx <- as_string(index(.data))
  idx_pos <- val_vars %in% idx
  idx_is_call <- dplyr::first(quos[idx_pos])
  key <- key(.data)
  red_key <-  key_flatten(key_distinct(key))
  if (is_false(any(idx_pos))) { # no index presented in the ...
    mvars <- measured_vars(.data)
    # if there's any measured variable in the ..., the time order will change.
    ordered <- !any(mvars %in% val_vars)
  } else if (quo_is_call(idx_is_call)) { # desc(index)
    fn <- call_name(idx_is_call)
    ordered <- fn != "desc"
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

#' @rdname tidyverse
#' @export
filter.tbl_ts <- function(.data, ...) {
  by_row(filter, .data, ordered = is_ordered(.data), interval = NULL, ...)
}

#' @rdname tidyverse
#' @export
slice.tbl_ts <- function(.data, ...) {
  pos <- enquos(...)
  if (length(pos) > 1) {
    abort("`slice()` only accepts one expression.")
  }
  pos_eval <- eval_tidy(expr(!! dplyr::first(pos)))
  exceed_rows(.data, max(pos_eval))
  ascending <- row_validate(pos_eval)
  int <- NULL
  if (is_min_gap_one(pos_eval)) int <- interval(.data)
  by_row(slice, .data, ordered = ascending, interval = int, ...)
}

row_validate <- function(x) {
  pos_dup <- anyDuplicated.default(x)
  if (any_not_equal_to_c(pos_dup, 0)) {
    abort(sprintf("Duplicated integers occur to the position of %i.", pos_dup))
  }
  is_ascending(x)
}

#' @param .drop `FALSE` returns a tsibble object as the input. `TRUE` drops a
#' tsibble and returns a tibble.
#'
#' @rdname tidyverse
#' @export
select.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) {
    return(select(as_tibble(.data), ...))
  }
  tsibble_select(.data, ...)
}

#' @rdname tidyverse
#' @export
rename.tbl_ts <- function(.data, ...) {
  tsibble_rename(.data, ...)
}

#' @rdname tidyverse
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

#' @rdname tidyverse
#' @export
transmute.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) {
    return(transmute(as_tibble(.data), ...))
  }
  lst_quos <- enquos(..., .named = TRUE)
  mut_data <- mutate(.data, !!! lst_quos)
  idx_key <- c(as_string(index(.data)), key_flatten(key(.data)))
  vec_names <- union(idx_key, names(lst_quos))
  select(mut_data, vec_names)
}

#' @rdname tidyverse
#' @export
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
summarise.tbl_ts <- function(.data, ..., .drop = FALSE) {
  if (.drop) {
    return(summarise(as_tibble(.data), ...))
  }
  idx <- index(.data)
  idx2 <- index2(.data)

  lst_quos <- enquos(..., .named = TRUE)
  idx2_chr <- quo_name(idx2)
  nonkey <- setdiff(
    names(lst_quos), 
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
  # mostly attempt to preserve the nesting structure
  key_less <- key(key_remove(.data, grps, validate = FALSE))
  flat_key <- key_flatten(key_less)
  # currently no way to set up nesting in `group_by()`
  add_key <- syms(setdiff(grps, c(flat_key, idx2_chr)))
  new_key <- c(key_less, add_key)

  build_tsibble_meta(
    sum_data, key = new_key, index = !! idx2, groups = grp_drop(grps, idx2_chr), 
    regular = reg, ordered = TRUE, interval = int
  )
}

#' @rdname tidyverse
#' @export
summarize.tbl_ts <- summarise.tbl_ts

#' @inheritParams dplyr::group_by
#' @importFrom dplyr grouped_df
#' @rdname tidyverse
#' @export
#' @examples
#' tourism %>%
#'   group_by(Region, State) %>%
#'   summarise(geo_trips = sum(Trips))
group_by.tbl_ts <- function(.data, ..., add = FALSE) {
  grped_tbl <- group_by(as_tibble(.data), ..., add = add)
  build_tsibble_meta(
    grped_tbl, key = key(.data), index = !! index(.data), 
    index2 = !! index2(.data), groups = groups(grped_tbl),
    regular = is_regular(.data), ordered = is_ordered(.data), 
    interval = interval(.data)
  )
}

#' Group by key variables
#'
#' @param .tbl A `tbl_ts` object.
#' @inheritParams dplyr::group_by_all
#' @export
#' @examples
#' tourism %>% 
#'   group_by_key()
group_by_key <- function(.tbl, .funs = list(), ...) {
  dplyr::group_by_at(.tbl, .vars = key_vars(.tbl), .funs = .funs, ...)
}

#' @rdname tidyverse
#' @export
ungroup.grouped_ts <- function(x, ...) {
  build_tsibble_meta(
    x, key = key(x), index = !! index(x), groups = id(),
    regular = is_regular(x), ordered = is_ordered(x), interval = interval(x)
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
