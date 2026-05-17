#' Expand a dataset to include other levels of aggregation
#' 
#' Uses the structural specification given in `.spec` to aggregate a time
#' series. A grouped structure is specified using `grp1 * grp2`, and a nested 
#' structure is specified via `parent / child`. Aggregating the key structure is
#' commonly used with forecast reconciliation to produce coherent forecasts over
#' some hierarchy.
#' 
#' The way in which the measured variables are aggregated is specified in a
#' similar way to how `[dplyr::summarise()]` is used.
#' 
#' @param .data A tsibble.
#' @param .spec The specification of aggregation structure.
#' @inheritParams dplyr::summarise
#' 
#' @examples 
#' library(tsibble)
#' tourism %>% 
#'   aggregate_key(Purpose * (State / Region), Trips = sum(Trips))
#' 
#' @export
aggregate_key <- function(.data, .spec, ...) {
  UseMethod("aggregate_key")
}

#' @export
aggregate_key.tbl_ts <- function(.data, .spec = NULL, ...) {
  require_package("graphvec")
  .spec <- enexpr(.spec)
  if(is.null(.spec)){
    kv <- syms(key_vars(.data))
    message(
      sprintf("Key structural specification not found, defaulting to `.spec = %s`",
              paste(kv, collapse = "*"))
    )
    .spec <- reduce(kv, call2, .fn = "*")
  }

  key_comb <- parse_agg_spec(.spec)
  
  idx <- index2_var(.data)
  intvl <- interval(.data)
  kd <- key_data(.data)
  cn <- colnames(.data)
  has_varied_index <- any(has_gaps(.data, .full = TRUE)[[".gaps"]]) && !is_ordered(.data)
  .data <- as_tibble(.data)
  
  kv <- unique(unlist(key_comb, recursive = FALSE))
  agg_dt <- map(unname(key_comb), function(x){
    gd <- group_data(group_by(.data, !!sym(idx), !!!set_names(map(x, function(.) expr(graphvec::agg_vec(!!sym(.)))), x)))
    agg_keys <- setdiff(kv, x)
    agg_cols <- rep(list(graphvec::agg_vec(NA, aggregated = TRUE)), length(agg_keys))
    gd[agg_keys] <- agg_cols
    gd[c(idx, kv, ".rows")]
  })
  agg_dt <- vctrs::vec_rbind(!!!agg_dt)
  .data <- dplyr::new_grouped_df(.data, groups = agg_dt)
  .data <- summarise(.data, ...)
  
  # Re-order columns into index, keys, values order
  .data <- .data[c(idx, kv, setdiff(colnames(.data), c(idx,kv)))]
  
  key_dt <- group_data(group_by(.data, !!!syms(kv)))
  .data <- ungroup(.data)
  
  # Return tsibble
  build_tsibble_meta(.data, key_data = key_dt, index = idx, 
                     index2 = as_string(idx), ordered = TRUE,
                     interval = intvl)
}

parse_agg_spec <- function(expr){
  # Key combinations
  tm <- stats::terms(new_formula(lhs = NULL, rhs = expr), env = empty_env())
  key_comb <- attr(tm, "factors")
  key_vars <- sub("^`(.*)`$", "\\1", rownames(key_comb))
  key_comb <- map(split(key_comb, col(key_comb)), function(x) key_vars[x!=0])
  if(attr(tm, "intercept")){
    key_comb <- c(list(chr()), key_comb)
  }
  unname(key_comb)
}


#' Temporal aggregation of the time index
#' 
#' Aggregate a tsibble with one or more levels of temporal aggregation. The 
#' granularity of the aggregated time indices is determined by the `.granule` 
#' argument, which is the chronons of the aggregated time indices.
#' 
#' @param data A tsibble.
#' @param .granule A list of mixtime time units to be the linear time chronons of
#'   the aggregated time indices. These time units are evaluated in the context 
#'   of the index variable's calendar.
#' @param .name The name of the additional key variable identifying the granule
#'   of the aggregated time indices.
#' @inheritParams dplyr::summarise
#' 
#' @seealso [aggregate_key()]
#' 
#' @examples
#' library(tsibble)
#' pedestrian %>% 
#'   aggregate_index(
#'     list(hour(12L), day(1L), month(1L), quarter(1L), year(1L)),
#'     Count = sum(Count)
#'   )
#' 
#' @name aggregate_index
#' @export
aggregate_index <- function(.data, .granule, ...){
  UseMethod("aggregate_index")
}

#' @rdname aggregate_index
#' @export
aggregate_index.tbl_ts <- function(.data, .granule = NULL, ..., .name = ".granule"){
  idx <- index_var(.data)
  cal <- mixtime::time_calendar(.data[[idx]])
  kv <- key_vars(.data)
  tu <- eval_tidy(enquo(.granule), data = cal, env = empty_env())
  
  # Temporal aggregations
  .data <- as_tibble(.data)
  agg_dt <- map(tu, function(x){
    gd <- group_data(group_by(.data, !!idx := mixtime::mixtime(!!sym(idx), x), !!!syms(kv), !!.name := mixtime::duration(1L, x)))
    gd[c(idx, kv, .name, ".rows")]
  })
  agg_dt <- vctrs::vec_rbind(!!!agg_dt)
  .data <- dplyr::new_grouped_df(mutate(.data, !!.name := NA), groups = agg_dt)
  .data <- summarise(.data, ..., .groups = "drop")
  
  # Re-order columns into index, keys, values order
  .data <- .data[c(idx, kv, .name, setdiff(colnames(.data), c(idx, kv, .name)))]
  
  # Return tsibble
  build_tsibble(.data, key = all_of(c(kv, .name)), index = idx, 
                index2 = as_string(idx), ordered = TRUE)
}