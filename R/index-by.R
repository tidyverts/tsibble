#' Group by time index
#'
#' `index_by()` is the counterpart of `group_by()` in temporal context, but it
#' only groups the time index. It captures the expression, and will be 
#' evaluated within the next function call. The following operation is applied 
#' to each group of the index, similar to `group_by()` but dealing with index only. 
#' Use `ungroup()` to remove the `index_by()`.
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
#' * A `index_by()`-ed tsibble is indicated by `@` followed by a promise in the 
#' "Groups" when displaying on the screen.
#' * Time index will not be collapsed by `summarise.tbl_ts`.
#' * The scoped variants of `summarise()` only operate on the non-key and 
#' non-index variables.
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
  if (is_false(has_length(exprs, 1))) {
    abort("`index_by()` only accepts one expression.")
  }
  build_tsibble(
    .data, key = key(.data), index = !! index(.data), index2 = exprs,
    groups = groups(.data), regular = is_regular(.data), validate = FALSE,
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

index_rename <- function(.data, .vars) {
  names_dat <- names(.data)
  names_vars <- names(.vars)
  idx_chr <- quo_text(index(.data))
  new_idx_chr <- names_vars[idx_chr == .vars]
  sym(new_idx_chr)
}

tsibble_rename <- function(.data, ...) {
  names_dat <- names(.data)
  val_vars <- tidyselect::vars_rename(names_dat, ...)
  names_vars <- names(val_vars)

  # index
  idx <- index_rename(.data, val_vars)

  # index2
  idx2 <- index2(.data)
  if (!is_empty(idx2)) {
    idx2_chr <- names(idx2)
    new_idx2_chr <- names_vars[idx2_chr == val_vars]
    first_expr <- idx2[[1]]
    if (is_symbol(first_expr)) {
      first_expr <- sym(new_idx2_chr)
    }
    idx2 <- exprs(!! new_idx2_chr := !! first_expr)
    attr(.data, "index2") <- idx2
  }

  # key (key of the same size (bf & af))
  new_key <- key_rename(.data, val_vars)
  # groups
  new_grp <- grp_rename(.data, val_vars)
  attr(.data, "vars") <- new_grp

  names(.data) <- names_vars
  vec_names <- union(names_vars, names(.data))
  # either key or index is present in ...
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index(vec_names, .data)
  val_key <- has_any_key(vec_names, .data)
  validate <- val_idx || val_key
  build_tsibble(
    .data, key = new_key, index = !! idx, index2 = idx2,
    groups = new_grp, regular = is_regular(.data), validate = FALSE, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}

tsibble_select <- function(.data, ...) {
  names_dat <- names(.data)
  val_vars <- tidyselect::vars_select(names_dat, ...)
  names_vars <- names(val_vars)
  sel_data <- select(as_tibble(.data), ...)

  # checking
  val_idx <- has_index(j = val_vars, x = .data)
  if (is_false(val_idx)) {
    abort(sprintf(
      "The `index` (`%s`) must not be dropped. Do you need `.drop = TRUE` to drop `tbl_ts`?",
      quo_text(index(.data))
    ))
  }
  
  # index
  idx <- index_rename(.data, val_vars)
  
  # index2
  idx2 <- index2(.data)
  if (!is_empty(idx2)) {
    idx2_chr <- names(idx2)
    new_idx2_chr <- names_vars[idx2_chr == val_vars]
    first_expr <- idx2[[1]]
    if (is_symbol(first_expr)) {
      first_expr <- sym(new_idx2_chr)
    }
    idx2 <- exprs(!! new_idx2_chr := !! first_expr)
  }
  
  # key (key of the reduced size (bf & af) but also different names)
  old_key <- key(.data)
  old_chr <- key_flatten(old_key)
  key_idx <- which(val_vars %in% old_chr)
  key_vars <- val_vars[key_idx]
  old_lgl <- FALSE
  if (!is_empty(old_key)) {
    old_lgl <- rep(is_nest(old_key), purrr::map(old_key, length))
  }
  new_lgl <- old_lgl[match(key_vars, old_chr)]
  new_key <- syms(names(key_vars)[!new_lgl])
  if (any(new_lgl)) {
    new_key <- c(list(syms(names(key_vars)[new_lgl])), new_key)
  }

  # groups
  new_grp <- grp_rename(.data, val_vars)
  
  vec_names <- union(names_vars, names(.data))
  # either key or index is present in ...
  # suggests that the operations are done on these variables
  # validate = TRUE to check if tsibble still holds
  val_idx <- has_index(vec_names, .data)
  val_key <- has_any_key(vec_names, .data)
  validate <- val_idx || val_key
  
  build_tsibble(
    sel_data, key = new_key, index = !! idx, index2 = idx2,
    groups = new_grp, regular = is_regular(.data), validate = validate, 
    ordered = is_ordered(.data), interval = interval(.data)
  )
}
