#' Tidyverse methods for tsibble
#'
#' @description
#' Current dplyr verbs that tsibble has support for:
#'
#' [dplyr::filter()], [dplyr::slice()], [dplyr::arrange()],
#' [dplyr::select()], [dplyr::transmute()], [dplyr::mutate()], [dplyr::relocate()],
#' [dplyr::summarise()], [dplyr::group_by()],
#' [dplyr::left_join()], [dplyr::right_join()], [dplyr::full_join()],
#' [dplyr::inner_join()], [dplyr::semi_join()], [dplyr::anti_join()],
#' [dplyr::nest_join()]
#'
#' Current tidyr verbs that tsibble has support for:
#'
#' [tidyr::gather()], [tidyr::spread()], [tidyr::nest()], [tidyr::fill()],
#' [tidyr::drop_na()]
#'
#' @section Column-wise verbs:
#' * The index variable cannot be dropped for a tsibble object.
#' * When any key variable is modified, a check on the validity of the resulting
#' tsibble will be performed internally.
#' * Use `as_tibble()` to convert tsibble to a general data frame.
#'
#' @section Row-wise verbs:
#' A warning is likely to be issued, if observations are not arranged in
#' past-to-future order.
#'
#' @section Join verbs:
#' Joining with other data sources triggers the check on the validity of the
#' resulting tsibble.
#'
#' @param .data,data A `tbl_ts`.
#' @param ... Same arguments accepted as its tidyverse generic.
#' @inheritParams dplyr::filter
#' @param .groups See [dplyr::summarise]
#'
#' @name tsibble-tidyverse
NULL

#' @export
arrange.tbl_ts <- function(.data, ...) {
  arr_data <- arrange(as_tibble(.data), ...)
  update_meta(arr_data, .data, ordered = FALSE, interval = interval(.data))
}

#' @export
arrange.grouped_ts <- arrange.tbl_ts

#' @export
select.tbl_ts <- function(.data, ...) {
  loc <- eval_select(expr(c(...)), .data)
  data_cp <- .data
  names(data_cp)[loc] <- names(loc)
  bind_tsibble(NextMethod(), data_cp, position = "after")
}

#' @export
select.grouped_ts <- select.tbl_ts

#' @rdname tsibble-tidyverse
#' @export
transmute.tbl_ts <- function(.data, ...) {
  bind_tsibble(NextMethod(), .data, position = "before")
}

#' @export
transmute.grouped_ts <- transmute.tbl_ts

#' @rdname tsibble-tidyverse
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' # Sum over sensors
#' pedestrian %>%
#'   index_by() %>%
#'   summarise(Total = sum(Count))
#' # shortcut
#' pedestrian %>%
#'   summarise(Total = sum(Count))
#' # Back to tibble
#' pedestrian %>%
#'   as_tibble() %>%
#'   summarise(Total = sum(Count))
#' @export
summarise.tbl_ts <- function(.data, ..., .groups = NULL) {
  # Unlike summarise.grouped_df(), summarise.tbl_ts() doesn't compute values for
  # empty groups. Bc information is unavailable over the time range for empty
  # groups.
  idx <- index(.data)
  idx2 <- index2(.data)

  # workaround for scoped variants
  lst_quos <- enquos(..., .named = TRUE)
  idx_chr <- as_string(idx)
  idx2_chr <- as_string(idx2)
  nonkey <- setdiff(names(lst_quos), c(key_vars(.data), idx_chr, idx2_chr))
  nonkey_quos <- lst_quos[nonkey]

  grped_data <- as_tibble(index_by(.data, !!idx2))
  sum_tbl <- summarise(grped_data, !!!nonkey_quos, .groups = .groups)

  groups_handler <- function(group_vars, .groups = NULL) {
    if (is_null(.groups) || .groups == "drop_last") {
      head(group_vars, -2) # remove index2 and last grp
    } else if (.groups == "drop") {
      character()
    } else if (.groups == "keep") {
      head(group_vars, -1)
    } # not sure how to handle "rowwise" for tsibble yet
  }

  sum_data <- grouped_df(sum_tbl,
    groups_handler(group_vars(grped_data), .groups = .groups))
  if (identical(idx, idx2)) int <- is_regular(.data) else int <- TRUE
  grps <- setdiff(group_vars(.data), idx2_chr)

  build_tsibble(
    sum_data,
    key = !!grps, index = !!idx2, ordered = TRUE, interval = int,
    validate = FALSE
  )
}

#' @export
summarise.grouped_ts <- summarise.tbl_ts

#' @importFrom dplyr group_by_drop_default
#' @export
group_by.tbl_ts <- function(.data, ..., .add = FALSE,
                            .drop = group_by_drop_default(.data)) {
  lst_quos <- enquos(..., .named = TRUE)
  grp_vars <- names(lst_quos)
  if (.add) grp_vars <- union(group_vars(.data), grp_vars)
  if (is_empty(grp_vars)) return(.data)

  index <- index_var(.data)
  if (index %in% grp_vars) {
    abort(c(
      sprintf("Column `%s` (index) can't be a grouping variable for a tsibble.", index), 
      i = "Did you mean `index_by()`?"))
  }

  grp_key <- identical(grp_vars, key_vars(.data)) &&
    identical(.drop, key_drop_default(.data))
  if (grp_key) {
    grped_tbl <- new_grouped_df(.data, groups = key_data(.data))
  } else {
    grped_tbl <- NextMethod()
  }
  build_tsibble(
    grped_tbl,
    key = !!key_vars(.data),
    key_data = if (grp_key) key_data(.data) else NULL,
    index = !!index(.data), index2 = !!index2(.data),
    ordered = is_ordered(.data), interval = interval(.data), validate = FALSE
  )
}

#' Group by key variables
#'
#' @description
#' \lifecycle{stable}
#'
#' @param .data A `tbl_ts` object.
#' @param ... Ignored.
#' @inheritParams dplyr::group_by
#' @export
#' @examples
#' tourism %>%
#'   group_by_key()
group_by_key <- function(.data, ..., .drop = key_drop_default(.data)) {
  group_by(.data, !!!key(.data), .drop = .drop)
}

#' @export
ungroup.grouped_ts <- function(x, ...) {
  tbl <- ungroup(as_tibble(x))
  build_tsibble(
    tbl,
    key_data = key_data(x), index = !!index(x),
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

#' @export
dplyr_row_slice.tbl_ts <- function(data, i, ..., preserve = FALSE) {
  tbl <- as_tibble(data)
  loc_df <- summarise(tbl, !!".loc" := list2(i))
  ascending <- all(map_lgl(loc_df[[".loc"]], validate_order))
  res <- dplyr_row_slice(tbl, i, ..., preserve = preserve)
  if (preserve) {
    update_meta2(res, data, ordered = ascending, interval = interval(data))
  } else {
    update_meta(res, data, ordered = ascending, interval = interval(data))
  }
}

#' @export
dplyr_row_slice.grouped_ts <- dplyr_row_slice.tbl_ts

#' @export
dplyr_col_modify.tbl_ts <- function(data, cols) {
  res <- dplyr_col_modify(as_tibble(data), cols)
  idx_chr <- index_var(data)
  if (is_false(idx_chr %in% names(res))) { # index has been removed
    abort(c(
      "Column `%s` (index) can't be removed for a tsibble.",
      i = sprintf("Do you need `as_tibble()` to work with data frame?"), idx_chr))
  }

  vec_names <- names(cols)
  # either key or index is present in `cols`
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index(vec_names, data)
  if (val_idx) interval <- TRUE else interval <- interval(data)

  val_key <- has_any_key(vec_names, data)
  if (val_key) {
    key_vars <- setdiff(names(res), measured_vars(data))
    data <- remove_key(data, key_vars)
  }

  validate <- val_idx || val_key
  if (validate) {
    res <- retain_tsibble(res, key_vars(data), index(data))
  }
  build_tsibble(
    res,
    key = !!key_vars(data),
    key_data = if (val_key) NULL else key_data(data), index = !!index(data),
    index2 = !!index2(data), ordered = is_ordered(data), interval = interval,
    validate = FALSE, .drop = is_key_dropped(data)
  )
}

#' @export
dplyr_col_modify.grouped_ts <- dplyr_col_modify.tbl_ts

#' @export
dplyr_reconstruct.tbl_ts <- function(data, template) {
  template <- rename_join_tsibble(data, template)
  update_meta(data, template,
    ordered = is_ordered(template), interval = is_regular(template),
    validate = TRUE)
}

#' @export
dplyr_reconstruct.grouped_ts <- function(data, template) {
  data <- grouped_df(data, group_vars(template))
  dplyr_reconstruct.tbl_ts(data, template)
}

rename_join_tsibble <- function(data, template) {
  nm <- names(template)
  key_idx_pos <- vec_match(c(index_var(template), key_vars(template)), nm)
  names(template)[key_idx_pos] <- names(data)[key_idx_pos]
  template
}
