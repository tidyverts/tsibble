#' Group by time index
#'
#' `index_by()` is the counterpart of `group_by()` in temporal context, but it
#' only groups the time index. It adds a new column and then group it. The 
#' following operation is applied to each group of the index, similar to 
#' `group_by()` but dealing with index only. `index_by()` + `summarise()` will
#' update the grouping index variable to be the new index. Use `ungroup()` or 
#' `index_by()` with no arguments to remove the index grouping vars.
#'
#' @param .data A `tbl_ts`.
#' @param ... 
#' * A single name-value pair of expression: a new index on LHS and the current 
#' index on RHS 
#' * An existing variable to be used as index
#' The index functions that can be used, but not limited:
#' * [lubridate::year]: yearly aggregation
#' * [yearquarter]: quarterly aggregation
#' * [yearmonth]: monthly aggregation
#' * [yearweek]: weekly aggregation
#' * [as.Date] or [lubridate::as_date]: daily aggregation
#' * [lubridate::ceiling_date], [lubridate::floor_date], or [lubridate::round_date]: 
#' sub-daily aggregation
#' * other index functions from other packages
#'
#' @details
#' * A `index_by()`-ed tsibble is indicated by `@` in the "Groups" when 
#' displaying on the screen.
#'
#' @rdname index-by
#' @export
#' @examples
#' # Monthly counts across sensors ----
#' monthly_ped <- pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   index_by(Year_Month = yearmonth(Date_Time)) %>%
#'   summarise(
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#' monthly_ped
#' index(monthly_ped)
#' 
#' # Using existing variable ----
#' pedestrian %>% 
#'   group_by(Sensor) %>% 
#'   index_by(Date) %>%
#'   summarise(
#'     Max_Count = max(Count),
#'     Min_Count = min(Count)
#'   )
#'
#' # Annual trips by Region and State ----
#' tourism %>% 
#'   index_by(Year = lubridate::year(Quarter)) %>% 
#'   group_by(Region, State) %>% 
#'   summarise(Total = sum(Trips))
index_by <- function(.data, ...) {
  UseMethod("index_by")
}

#' @export
index_by.tbl_ts <- function(.data, ...) {
  exprs <- enexprs(..., .named = TRUE)
  if (is_empty(exprs)) {
    attr(.data, "index2") <- index(.data)
    return(.data)
  }
  if (is_false(has_length(exprs, 1))) {
    abort("`index_by()` only accepts one expression.")
  }
  # ungroup() protect the index class
  tbl <- mutate(ungroup(.data), !!! exprs)
  idx2 <- sym(names(exprs)[1])
  build_tsibble(
    tbl, key = key(.data), index = !! index(.data), index2 = !! idx2,
    groups = groups(.data), regular = is_regular(.data), validate = FALSE,
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

index_rename <- function(.data, .vars) {
  names <- names(.vars)
  idx_chr <- quo_text(index(.data))
  new_idx_chr <- names[idx_chr == .vars]
  sym(new_idx_chr)
}

index2_rename <- function(.data, .vars) {
  names <- names(.vars)
  idx_chr <- quo_text(index2(.data))
  new_idx_chr <- names[idx_chr == .vars]
  sym(new_idx_chr)
}

index2_update <- function(.data, .vars) {
  chr <- intersect(quo_name(index2(.data)), .vars)
  if (is_empty(chr)) {
    index(.data)
  } else {
    sym(chr)
  }
}

tsibble_rename <- function(.data, ...) {
  names_dat <- names(.data)
  val_vars <- tidyselect::vars_rename(names_dat, ...)

  # index
  idx <- index_rename(.data, val_vars)
  # index2
  idx2 <- index2_rename(.data, val_vars)
  attr(.data, "index2") <- idx2
  # key (key of the same size (bf & af))
  new_key <- key_rename(.data, val_vars)
  # groups
  new_grp <- grp_rename(.data, val_vars)
  attr(.data, "vars") <- new_grp

  names(.data) <- names(val_vars)
  build_tsibble(
    .data, key = new_key, index = !! idx, index2 = !! idx2,
    groups = new_grp, regular = is_regular(.data), validate = FALSE, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

tsibble_select <- function(.data, ..., validate = TRUE) {
  dots <- c(enquos(...), new_quosure(index(.data)))
  names_dat <- names(.data)
  val_vars <- tidyselect::vars_select(names_dat, !!! dots)
  sel_data <- select(as_tibble(.data), !!! val_vars)

  # checking
  # val_idx <- has_index(j = val_vars, x = .data)
  # if (is_false(val_idx)) {
  #   abort(sprintf(
  #     "The `index` (`%s`) must not be dropped, do you want `.drop = TRUE` to drop `tbl_ts`?",
  #     quo_text(index(.data))
  #   ))
  # }
  
  # index
  idx <- index_rename(.data, val_vars)
  # index2
  idx2 <- index2_rename(.data, val_vars)
  # key (key of the reduced size (bf & af) but also different names)
  key_vars <- val_vars[val_vars %in% key_vars(.data)]
  tmp_data <- key_remove(.data, key_vars, validate = FALSE)
  new_key <- key_rename(tmp_data, key_vars)
  # groups
  new_grp <- grp_rename(.data, val_vars)
  
  if (validate) {
    vec_names <- union(names(val_vars), names(.data))
    # either key or index is present in ...
    # suggests that the operations are done on these variables
    # validate = TRUE to check if tsibble still holds
    # val_idx <- has_index(vec_names, .data)
    # val_key <- has_any_key(vec_names, .data)
    # validate <- val_idx || val_key
    validate <- has_any_key(vec_names, .data)
  }
  
  build_tsibble(
    sel_data, key = new_key, index = !! idx, index2 = !! idx2,
    groups = new_grp, regular = is_regular(.data), validate = validate, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}
