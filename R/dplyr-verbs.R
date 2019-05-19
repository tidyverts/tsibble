#' Tidyverse methods for tsibble
#'
#' * `arrange()`: if not arranging key and index in past-to-future order, a warning is
#' likely to be issued.
#' * `slice()`: if row numbers are not in ascending order, a warning is likely to
#' be issued.
#' * `select()`: keeps the variables you mention as well as the index.
#' * `transmute()`: keeps the variable you operate on, as well as the index and key.
#' * `summarise()` reduces a sequence of values over time instead of a single summary,
#' as well as dropping empty keys/groups.
#' * `unnest()` requires argument `key = NULL` to get back to a tsibble.
#'
#' @param .data A `tbl_ts`.
#' @param ... Same arguments accepted as its dplyr generic.
#' @inheritParams dplyr::arrange
#' @details
#' Column-wise verbs, including `select()`, `transmute()`, `summarise()`,
#' `mutate()` & `transmute()`, keep the time context hanging around. That is,
#' the index variable cannot be dropped for a tsibble. If any key variable
#' is changed, it will validate whether it's a tsibble internally. Use `as_tibble()`
#' to leave off the time context.
#'
#' @name tsibble-tidyverse
#' @rdname tsibble-tidyverse
#' @export
arrange.tbl_ts <- function(.data, ...) {
  arr_data <- NextMethod()
  update_meta(arr_data, .data, ordered = FALSE, interval = interval(.data))
}

#' @export
arrange.grouped_ts <- arrange.tbl_ts

#' @rdname tsibble-tidyverse
filter.tbl_ts <- function(.data, ..., .preserve = FALSE) {
  by_row(filter, .data, ordered = is_ordered(.data),
    interval = is_regular(.data), ..., .preserve = .preserve
  )
}

#' @rdname tsibble-tidyverse
#' @export
slice.tbl_ts <- function(.data, ..., .preserve = FALSE) {
  pos <- enquos(...)
  if (length(pos) > 1) {
    abort("`slice()` only accepts one expression.")
  }
  pos_df <- summarise(as_tibble(.data), !! ".pos_col" := list2(!! pos[[1]]))
  ascending <- all(map_lgl(pos_df[[".pos_col"]], validate_order))
  by_row(slice, .data, ordered = ascending, interval = is_regular(.data), ...,
    .preserve = .preserve)
}

#' @rdname tsibble-tidyverse
#' @export
select.tbl_ts <- function(.data, ...) {
  lst_quos <- enquos(...)
  lst_exprs <- map(lst_quos, quo_get_expr)
  idx_chr <- index_var(.data)
  rm_index <- sym(paste0("-", idx_chr))
  if (any(lst_exprs == rm_index)) {
    abort(sprintf(
      "Column `%s` (index) can't be removed.\nDo you need `as_tibble()` to work with data frame?",
      idx_chr
    ))
  }

  named <- list_is_named(lst_quos)
  .data <- rename_tsibble(.data, !!! lst_quos[named])

  lst_env <- map(lst_quos, quo_get_env)[named]
  lst_quos[named] <- as_quosures(names(lst_quos)[named], env = lst_env)
  select_tsibble(.data, !!! lst_quos)
}

#' @export
select.grouped_ts <- select.tbl_ts

#' @rdname tsibble-tidyverse
#' @export
rename.tbl_ts <- function(.data, ...) {
  rename_tsibble(.data, ...)
}

#' @export
rename.grouped_ts <- rename.tbl_ts

#' @rdname tsibble-tidyverse
#' @export
mutate.tbl_ts <- function(.data, ...) {
  # mutate returns lst_ts without attributes, coerce to tbl_df first
  mut_data <- mutate(as_tibble(.data), ...)

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
  if (val_idx) {
    interval <- TRUE
  } else {
    interval <- interval(.data)
  }
  if (val_key) {
    key_vars <- setdiff(names(mut_data), measured_vars(.data))
    .data <- remove_key(.data, key_vars)
  }
  validate <- val_idx || val_key
  if (validate) {
    mut_data <- retain_tsibble(mut_data, key(.data), index(.data))
  }
  build_tsibble(
    mut_data, key = !! key_vars(.data), index = !! index(.data),
    index2 = !! index2(.data), ordered = is_ordered(.data), interval = interval,
    validate = FALSE, .drop = is_key_dropped(.data)
  )
}

#' @rdname tsibble-tidyverse
#' @export
transmute.tbl_ts <- function(.data, ...) {
  lst_quos <- enquos(..., .named = TRUE)
  mut_data <- mutate(.data, !!! lst_quos)
  idx_key <- c(key_vars(.data), index_var(.data))
  vec_names <- union(idx_key, names(lst_quos))
  select_tsibble(mut_data, !!! vec_names, validate = FALSE)
}

#' @export
transmute.grouped_ts <- function(.data, ...) {
  res <- NextMethod()
  # keeping index and key
  idx_key <- c(key_vars(.data), index_var(.data))
  tsbl <- select_tsibble(ungroup(.data), !!! idx_key, validate = FALSE)
  dplyr::bind_cols(tsbl, res[, !(names(res) %in% names(tsbl))])
}

#' @rdname tsibble-tidyverse
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' # Sum over sensors ----
#' pedestrian %>%
#'   summarise(Total = sum(Count))
#' # Back to tibble
#' pedestrian %>%
#'   as_tibble() %>%
#'   summarise(Total = sum(Count))
#' @export
summarise.tbl_ts <- function(.data, ...) {
  # Unlike summarise.grouped_df(), summarise.tbl_ts() doesn't compute values for
  # empty groups. Bc information is unavailable over the time range for empty
  # groups.
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
  sum_data <-
    group_by(
      summarise(grped_data, !!! nonkey_quos),
      !!! grps[-((len_grps - 1):len_grps)] # remove index2 and last grp
    )
  if (identical(idx, idx2)) {
    int <- is_regular(.data)
  } else {
    int <- TRUE
  }
  grps <- setdiff(group_vars(.data), as_string(idx2))

  build_tsibble(
    sum_data, key = !! grps, index = !! idx2, ordered = TRUE, interval = int,
    validate = FALSE
  )
}

#' @importFrom dplyr group_by_drop_default
#' @export
group_by.tbl_ts <- function(.data, ..., add = FALSE,
  .drop = group_by_drop_default(.data)) {
  lst_quos <- enquos(..., .named = TRUE)
  grp_vars <- names(lst_quos)
  if (add) {
    grp_vars <- union(grp_vars, group_vars(.data))
  }
  if (is_empty(grp_vars)) return(.data)
  index <- index_var(.data)
  if (index %in% grp_vars) {
    err <- sprintf("Column `%s` (index) can't be a grouping variable for a tsibble.\n", index)
    hint <- "Did you mean `index_by()`?"
    abort(paste0(err, hint))
  }

  grped_tbl <- NextMethod()
  if (.drop) { # needs to drop key too
    build_tsibble(
      grped_tbl, key = !! key_vars(.data), index = !! index(.data),
      index2 = !! index2(.data), ordered = is_ordered(.data),
      interval = interval(.data), validate = FALSE
    )
  } else {
    build_tsibble(
      grped_tbl, key_data = key_data(.data), index = !! index(.data),
      index2 = !! index2(.data), ordered = is_ordered(.data),
      interval = interval(.data), validate = FALSE
    )
  }
}

#' Group by key variables
#'
#' @param .data A `tbl_ts` object.
#' @param ... Ignored.
#' @inheritParams dplyr::group_by
#' @export
#' @examples
#' tourism %>%
#'   group_by_key()
group_by_key <- function(.data, ..., .drop = key_drop_default(.data)) {
  group_by(.data, !!! key(.data), .drop = .drop)
}

#' @export
ungroup.grouped_ts <- function(x, ...) {
  tbl <- ungroup(as_tibble(x))
  build_tsibble(
    tbl, key_data = key_data(x), index = !! index(x),
    ordered = is_ordered(x), interval = interval(x), validate = FALSE
  )
}

#' @export
ungroup.tbl_ts <- function(x, ...) {
  attr(x, "index2") <- index_var(x)
  x
}

distinct.tbl_ts <- function(.data, ...) {
  dplyr::distinct(as_tibble(.data), ...)
}
