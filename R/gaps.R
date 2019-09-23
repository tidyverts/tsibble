globalVariables(c(".", ".gaps"))

#' Turn implicit missing values into explicit missing values
#'
#' \lifecycle{stable}
#'
#' @param .data A tsibble.
#' @param ... A set of name-value pairs. The values provided will only replace
#' missing values that were marked as "implicit", and will leave previously
#' existing `NA` untouched.
#' * empty: filled with default `NA`.
#' * filled by values or functions.
#' @param .full `FALSE` inserts `NA` for each keyed unit within its own period. `TRUE`
#' fills `NA` over the entire time span of the data (a.k.a. fully balanced panel).
#' Other options: `start()` or `end()` at the same time.
#'
#' @family implicit gaps handling
#' @seealso [tidyr::fill], [tidyr::replace_na] for handling missing values `NA`.
#' @export
#' @examples
#' harvest <- tsibble(
#'   year = c(2010, 2011, 2013, 2011, 2012, 2014),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = fruit, index = year
#' )
#'
#' # gaps as default `NA`
#' fill_gaps(harvest, .full = TRUE)
#' fill_gaps(harvest, .full = start())
#' fill_gaps(harvest, .full = end())
#' full_harvest <- fill_gaps(harvest, .full = FALSE)
#' full_harvest
#'
#' # replace gaps with a specific value
#' harvest %>%
#'   fill_gaps(kilo = 0L)
#'
#' # replace gaps using a function by variable
#' harvest %>%
#'   fill_gaps(kilo = sum(kilo))
#'
#' # replace gaps using a function for each group
#' harvest %>%
#'   group_by_key() %>%
#'   fill_gaps(kilo = sum(kilo))
#'
#' # leaves existing `NA` untouched
#' harvest[2, 3] <- NA
#' harvest %>%
#'   group_by_key() %>%
#'   fill_gaps(kilo = sum(kilo, na.rm = TRUE))
#'
#' # replace NA
#' pedestrian %>%
#'   group_by_key() %>%
#'   fill_gaps(Count = as.integer(median(Count)))
#'
#' if (!requireNamespace("tidyr", quietly = TRUE)) {
#'   stop("Please install the 'tidyr' package to run these following examples.")
#' }
#' # use fill() to fill `NA` by previous/next entry
#' pedestrian %>%
#'   group_by_key() %>%
#'   fill_gaps() %>% 
#'   tidyr::fill(Count, .direction = "down")
fill_gaps <- function(.data, ..., .full = FALSE) {
  UseMethod("fill_gaps")
}

#' @export
fill_gaps.data.frame <- function(.data, ...) {
  abort("Do you need `tidyr::complete()` for a `tbl_df`/`data.frame`?")
}

#' @export
fill_gaps.tbl_ts <- function(.data, ..., .full = FALSE) {
  nrows <- vec_size(.data)
  if (nrows == 0L || nrows == 1L) return(.data)

  gap_data <- scan_gaps(.data, .full = !!enquo(.full))
  lst_exprs <- enquos(..., .named = TRUE)
  if (vec_size(gap_data) == 0 && !is_empty(lst_exprs)) return(.data)

  if (!is_empty(lst_exprs)) { # any replacement
    # error handling
    vars_select(measured_vars(.data), !!!names(lst_exprs))
    replaced_df <- ungroup(summarise(as_tibble(.data), !!!lst_exprs))
    by_name <- intersect(names(gap_data), names(replaced_df))
    if (is_empty(by_name)) { # by value
      gap_data <- mutate(gap_data, !!!replaced_df)
    } else { # by function
      gap_data <- left_join(gap_data, replaced_df, by = by_name)
    }
  }
  grps <- groups(.data)
  full_data <- group_by(vec_rbind(as_tibble(gap_data), .data), !!!grps)
  full_data <- full_data[names(.data)] # keep the original order
  update_meta(full_data, .data, ordered = NULL, interval = interval(.data))
}

#' Scan a tsibble for implicit missing observations
#'
#' @inheritParams count_gaps
#' @family implicit gaps handling
#' @export
#' @examples
#' scan_gaps(pedestrian)
scan_gaps <- function(.data, .full = FALSE) {
  UseMethod("scan_gaps")
}

#' @export
scan_gaps.tbl_ts <- function(.data, .full = FALSE) {
  .full <- quo_get_expr(enquo(.full))
  int <- interval(.data)
  idx <- index(.data)
  idx_chr <- as_string(idx)
  if (unknown_interval(int) || !is_regular(.data)) {
    return(.data[0L, c(key_vars(.data), idx_chr)])
  }

  key <- key(.data)
  keyed_tbl <- new_grouped_df(.data, groups = key_data(.data))
  if (is_true(.full)) {
    idx_full <- seq_generator(keyed_tbl[[idx_chr]], int)
    sum_data <-
      summarise(keyed_tbl, !!idx_chr := list2(tibble(!!idx_chr := idx_full)))
  } else if (is_false(.full)) {
    sum_data <- summarise(keyed_tbl,
      !!idx_chr := list2(tibble(!!idx_chr := seq_generator(!!idx, int)))
    )
  } else if (.full == sym("start()")) {
    start <- min(keyed_tbl[[idx_chr]])
    sum_data <- summarise(keyed_tbl,
      !!idx_chr := list2(tibble(
        !!idx_chr := seq_generator(c(start, max(!!idx)), int)
      ))
    )
  } else if (.full == sym("end()")) {
    end <- max(keyed_tbl[[idx_chr]])
    sum_data <- summarise(keyed_tbl,
      !!idx_chr := list2(tibble(
        !!idx_chr := seq_generator(c(min(!!idx), end), int)
      ))
    )
  } else {
    abort_invalid_full_arg()
  }
  ref_data <- unwrap(sum_data, !!idx)
  if (vec_size(ref_data) == vec_size(.data)) {
    .data[0L, c(key_vars(.data), idx_chr)]
  } else {
    gap_data <- anti_join(ref_data, .data, by = c(key_vars(.data), idx_chr))
    update_meta(gap_data, .data, ordered = NULL, interval = interval(.data))
  }
}

#' Count implicit gaps
#'
#' @inheritParams fill_gaps
#' @param .name Strings to name new columns.
#'
#' @family implicit gaps handling
#' @export
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * ".from": the starting time point of the gap
#' * ".to": the ending time point of the gap
#' * ".n": the number of implicit missing observations during the time period
#' @examples
#' ped_gaps <- pedestrian %>%
#'   count_gaps(.full = TRUE)
#' ped_gaps
#' if (!requireNamespace("ggplot2", quietly = TRUE)) {
#'   stop("Please install the ggplot2 package to run these following examples.")
#' }
#' library(ggplot2)
#' ggplot(ped_gaps, aes(x = Sensor, colour = Sensor)) +
#'   geom_linerange(aes(ymin = .from, ymax = .to)) +
#'   geom_point(aes(y = .from)) +
#'   geom_point(aes(y = .to)) +
#'   coord_flip() +
#'   theme(legend.position = "bottom")
count_gaps <- function(.data, .full = FALSE, .name = c(".from", ".to", ".n")) {
  int <- interval(.data)
  idx <- index(.data)

  gap_data <- scan_gaps(.data, .full = !!enquo(.full))
  if (vec_size(gap_data) == 0L) {
    data_key <- .data[0L, key_vars(.data)]
    idx_vals <- .data[[as_string(idx)]][0L]
    out <- vec_cbind(data_key, tbl_gaps(idx_vals, idx_vals, .name = .name))
    return(out)
  }

  idx_full <- seq_generator(.data[[as_string(idx)]], int)
  grped_tbl <- new_grouped_df(gap_data, groups = key_data(gap_data))
  lst_out <- summarise(grped_tbl,
    !!".gaps" := list2(tbl_gaps(!!idx, idx_full, .name = .name)))

  out <- unwrap(lst_out, .gaps)
  tibble(!!!out)
}

#' Does a tsibble have implicit gaps in time?
#'
#' @inheritParams count_gaps
#' @export
#' @family implicit gaps handling
#' @return A tibble contains "key" variables and new column `.gaps` of `TRUE`/`FALSE`.
#' @examples
#' harvest <- tsibble(
#'   year = c(2010, 2011, 2013, 2011, 2012, 2013),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = fruit, index = year
#' )
#' has_gaps(harvest)
#' has_gaps(harvest, .full = TRUE)
#' has_gaps(harvest, .full = start())
#' has_gaps(harvest, .full = end())
has_gaps <- function(.data, .full = FALSE, .name = ".gaps") {
  stopifnot(has_length(.name, 1))
  if (!is_regular(.data) || vec_size(.data) == 0L) {
    key_data <- key_data(.data)[key_vars(.data)]
    return(tibble(!!!key_data, !!.name := FALSE))
  }

  .full <- quo_get_expr(enquo(.full))
  int <- interval(.data)
  idx <- index(.data)
  idx_chr <- as_string(idx)
  grped_tbl <- new_grouped_df(.data, groups = key_data(.data))
  if (is_true(.full)) {
    idx_full <- seq_generator(.data[[idx_chr]], int)
    res <- summarise(grped_tbl,
      !!.name := (length(idx_full) - length(!!idx)) > 0)
  } else if (is_false(.full)) {
    res <- summarise(grped_tbl,
      !!.name := (length(seq_generator(!!idx, int)) - length(!!idx)) > 0
    )
  } else if (.full == sym("start()")) {
    start <- min(.data[[idx_chr]])
    res <- summarise(grped_tbl,
      !!.name := (length(seq_generator(c(start, max(!!idx)), int)) - length(!!idx)) > 0
    )
  } else if (.full == sym("end()")) {
    end <- max(.data[[idx_chr]])
    res <- summarise(grped_tbl,
      !!.name := (length(seq_generator(c(min(!!idx), end), int)) - length(!!idx)) > 0
    )
  } else {
    abort_invalid_full_arg()
  }
  tibble(!!!res)
}

tbl_gaps <- function(x, y, .name = c(".from", ".to", ".n")) {
  stopifnot(has_length(.name, 3))
  len_x <- vec_size(x)
  len_y <- vec_size(y)
  if (len_y < len_x) {
    abort(sprintf(
      "`length(x)` (%d) must not be greater than `length(y)` (%d).",
      len_x, len_y
    ))
  }
  if (len_x == 0) {
    return(tibble(!!.name[1] := y, !!.name[2] := y, !!.name[3] := integer()))
  }
  gap_vec <- logical(length = len_y)
  gap_vec[vec_match(x, y)] <- TRUE
  gap_rle <- rle(gap_vec)
  lgl_rle <- as.logical(gap_rle$values)
  gap_idx <- gap_rle$lengths
  to <- cumsum(gap_idx)
  from <- vec_c(1, to[-vec_size(to)] + 1)
  nobs <- gap_idx[lgl_rle]
  tibble(
    !!.name[1] := y[from][lgl_rle],
    !!.name[2] := y[to][lgl_rle],
    !!.name[3] := nobs
  )
}

seq_generator <- function(x, interval = NULL) {
  if (is_empty(x)) return(x)

  min_x <- min(x)
  max_x <- max(x)
  if (is_null(interval)) {
    interval <- interval_pull(x)
  }
  tunit <- default_time_units(interval)
  if (tunit == 0) return(x)

  res <- tryCatch(
    seq(min_x, max_x, tunit),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is_null(res)) return(res)

  # no seq.* available
  res2 <- tryCatch(
    min_x + seq.int(0, as.double(max_x - min_x), tunit),
    error = function(e) {
      e$call <- NULL
      e$message <- sprintf("Neither `+` nor `seq()` are defined for class %s", class(x)[1L])
      stop(e)
    }
  )
  if (inherits(x, "hms")) { # workaround for hms
    res2 <- hms::as_hms(res2)
  }
  res2
}

unwrap <- function(.data, .col) {
  lst_col <- vars_pull(names(.data), !!enquo(.col))
  res <- .data
  row_indices <- rep.int(
    vec_seq_along(.data), vapply(.data[[lst_col]], vec_size, integer(1))
  )
  res <- vec_slice(res, row_indices)[setdiff(names(.data), lst_col)]
  vec_cbind(res, vec_rbind(!!!.data[[lst_col]]))
}

abort_invalid_full_arg <- function() {
  abort("`.full` only accepts `TRUE`, `FALSE`, `start()`, or `end()`.")
}
