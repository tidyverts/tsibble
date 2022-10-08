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
#' @param .full
#' * `FALSE` inserts `NA` for each keyed unit within its own period.
#' * `TRUE` fills `NA` over the entire time span of the data (a.k.a. fully balanced panel).
#' * `start()` pad `NA` to the same starting point (i.e. `min(<index>)`) across units.
#' * `end()` pad `NA` to the same ending point (i.e. `max(<index>)`) across units.
#' @param .start,.end Set custom starting/ending time that allows to expand the
#' existing time spans.
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
#' fill_gaps(harvest, .start = 2009, .end = 2016)
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
fill_gaps <- function(.data, ..., .full = FALSE, .start = NULL, .end = NULL) {
  UseMethod("fill_gaps")
}

#' @export
fill_gaps.data.frame <- function(.data, ...) {
  abort("Do you need `tidyr::complete()` for a `tbl_df`/`data.frame`?")
}

#' @export
fill_gaps.tbl_ts <- function(.data, ..., .full = FALSE,
  .start = NULL, .end = NULL) {
  nrows <- vec_size(.data)
  if (nrows == 0L || nrows == 1L) return(.data)

  gap_data <- scan_gaps(.data, .full = !!enquo(.full),
    .start = .start, .end = .end)
  lst_exprs <- enquos(..., .named = TRUE)
  if (vec_size(gap_data) == 0 && !is_empty(lst_exprs)) return(.data)

  if (!is_empty(lst_exprs)) { # any replacement
    # error handling
    eval_select(names(lst_exprs), .data)
    replaced_df <- ungroup(summarise(as_tibble(.data), !!!lst_exprs))
    by_name <- intersect(names(gap_data), names(replaced_df))
    if (is_empty(by_name)) { # by value
      gap_data <- mutate(gap_data, !!!replaced_df)
    } else { # by function
      gap_data <- left_join(gap_data, replaced_df, by = by_name)
    }
  }
  grps <- groups(.data)
  full_data <- group_by(vec_rbind(as_tibble(gap_data), as_tibble(.data)),
    !!!grps)
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
scan_gaps <- function(.data, .full = FALSE, .start = NULL, .end = NULL) {
  UseMethod("scan_gaps")
}

#' @export
scan_gaps.tbl_ts <- function(.data, .full = FALSE, .start = NULL, .end = NULL) {
  .full <- quo_get_expr(enquo(.full))
  int <- interval(.data)
  idx <- index(.data)
  idx_chr <- as_string(idx)
  if (unknown_interval(int) || !is_regular(.data)) {
    data0 <- vec_slice(.data, 0)
    return(data0[c(key_vars(.data), idx_chr)])
  }
  int <- default_time_units(int)

  key <- key(.data)
  is_start <- !is_null(.start)
  is_end <- !is_null(.end)
  keyed_tbl <- new_grouped_df(.data, groups = key_data(.data))
  if (is_start) {
    start <- min(keyed_tbl[[idx_chr]])
    if (.start > start) {
      abort(sprintf(
        "Argument `.start` can only take a value earlier than %s.", start))
    }
    keyed_lst <- summarise(keyed_tbl,
      !!idx_chr := list2(!!idx_chr := seq_generator(c(.start, max(!!idx)), int)))
    keyed_tbl <- group_by(unwrap(keyed_lst, !!idx), !!!key)
  }
  if (is_end) {
    end <- max(keyed_tbl[[idx_chr]])
    if (.end < end) {
      abort(sprintf(
        "Argument `.end` can only take a value later than %s.", end))
    }
    keyed_lst <- summarise(keyed_tbl,
      !!idx_chr := list2(!!idx_chr := seq_generator(c(min(!!idx), .end), int)))
    keyed_tbl <- group_by(unwrap(keyed_lst, !!idx), !!!key)
  }
  if (is_start && is_end) {
    ref_data <- keyed_tbl
  } else {
    if (is_true(.full)) {
      idx_full <- seq_generator(keyed_tbl[[idx_chr]], int)
      sum_data <-
        summarise(keyed_tbl, !!idx_chr := list2(!!idx_chr := idx_full))
    } else if (is_false(.full)) {
      sum_data <- summarise(keyed_tbl,
        !!idx_chr := list2(!!idx_chr := seq_generator(!!idx, int)))
    } else if (call_name(.full) == "start") {
      abort_if_args_present(.full)
      start <- min(keyed_tbl[[idx_chr]])
      sum_data <- summarise(keyed_tbl,
        !!idx_chr := list2(!!idx_chr := seq_generator(c(start, max(!!idx)), int)))
    } else if (call_name(.full) == "end") {
      abort_if_args_present(.full)
      end <- max(keyed_tbl[[idx_chr]])
      sum_data <- summarise(keyed_tbl,
        !!idx_chr := list2(!!idx_chr := seq_generator(c(min(!!idx), end), int)))
    } else {
      abort_invalid_full_arg()
    }
    ref_data <- unwrap(sum_data, !!idx)
  }
  if (vec_size(ref_data) == vec_size(.data)) {
    data0 <- vec_slice(.data, 0)
    data0[c(key_vars(.data), idx_chr)]
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
count_gaps <- function(.data, .full = FALSE, .name = c(".from", ".to", ".n"),
  .start = NULL, .end = NULL) {
  int <- default_time_units(interval(.data))
  idx <- index(.data)

  gap_data <- scan_gaps(.data, .full = !!enquo(.full),
    .start = .start, .end = .end)
  if (vec_size(gap_data) == 0L) {
    data0 <- vec_slice(.data, 0)
    data_key <- data0[key_vars(.data)]
    idx_vals <- .data[[as_string(idx)]][0L]
    out <- vec_cbind(data_key, tbl_gaps(idx_vals, idx_vals, .name = .name))
    return(out)
  }

  idx_full <- seq_generator(.data[[as_string(idx)]], int)
  grped_tbl <- new_grouped_df(gap_data, groups = key_data(gap_data))
  lst_out <- summarise(grped_tbl,
    !!".gaps" := tbl_gaps(!!idx, idx_full, .name = .name))

  vec_cbind(lst_out[key_vars(.data)], vec_cbind(!!!lst_out$.gaps))
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
has_gaps <- function(.data, .full = FALSE, .name = ".gaps",
  .start = NULL, .end = NULL) {
  stopifnot(has_length(.name, 1))
  if (!is_regular(.data) || vec_size(.data) == 0L) {
    key_data <- key_data(.data)[key_vars(.data)]
    return(tibble(!!!key_data, !!.name := FALSE))
  }

  .full <- enquo(.full)
  int <- default_time_units(interval(.data))
  idx <- index(.data)
  idx_chr <- as_string(idx)
  if (!is_null(.start) || !is_null(.end)) {
    gaps_data <- scan_gaps(.data, .full = !!.full, .start = .start, .end = .end)
    grped_tbl <- grouped_df(gaps_data, key_vars(.data), drop = FALSE)
    res <- summarise(grped_tbl, !!.name := length(!!idx) > 0)
  } else {
    grped_tbl <- new_grouped_df(.data, groups = key_data(.data))
    lgl <- !quo_is_symbol(.full) & !quo_is_call(.full)
    if (lgl || quo_is_symbol(.full)) {
      .full <- eval_tidy(.full)
      if (is_true(.full)) {
        idx_full <- seq_generator(.data[[idx_chr]], int)
        res <- summarise(grped_tbl,
          !!.name := (length(idx_full) - length(!!idx)) > 0)
      } else if (is_false(.full)) {
        res <- summarise(grped_tbl,
          !!.name := (length(seq_generator(!!idx, int)) - length(!!idx)) > 0
        )
      } else {
        abort_invalid_full_arg()
      }
    } else {
      if (call_name(.full) == "start") {
        abort_if_args_present(.full)
        start <- min(.data[[idx_chr]])
        res <- summarise(grped_tbl,
          !!.name := (length(seq_generator(c(start, max(!!idx)), int)) - length(!!idx)) > 0
        )
      } else if (call_name(.full) == "end") {
        abort_if_args_present(.full)
        end <- max(.data[[idx_chr]])
        res <- summarise(grped_tbl,
          !!.name := (length(seq_generator(c(min(!!idx), end), int)) - length(!!idx)) > 0
        )
      } else {
        abort_invalid_full_arg()
      }
    }
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
  new_data_frame(list2(
    !!.name[1] := y[from][lgl_rle],
    !!.name[2] := y[to][lgl_rle],
    !!.name[3] := nobs
  ))
}

seq_generator <- function(x, time_units = NULL, length_out = NULL) {
  if (is_empty(x)) return(x)

  if (time_units == 0) return(x)

  min_x <- min(x)
  seq_call <- quote(seq(from = min_x, to = max(x), by = time_units, 
    length.out = length_out))
  if (!is.null(length_out)) {
    seq_call <- call_modify(seq_call, to = zap())
  }
  res <- tryCatch(
    eval_bare(seq_call),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(res)) return(res)

  # no seq.* available
  seq_call2 <- quote(seq.int(from = 0, to = as.double(max(x) - min_x),
    by = time_units, length.out = length_out))
  if (!is.null(length_out)) {
    seq_call2 <- call_modify(seq_call2, to = zap())
  }
  msg <- sprintf("Neither `+` nor `seq()` are defined for class %s", class(x)[1L])
  res2 <- tryCatch(
    min_x + eval_bare(seq_call2),
    error = function(e) {
      e$call <- NULL
      e$message <- msg
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
  vec_cbind(res, !!lst_col := vec_c(!!!.data[[lst_col]], .name_spec = zap()))
}

abort_invalid_full_arg <- function() {
  abort("`.full` only accepts `TRUE`, `FALSE`, `start()`, or `end()`.")
}

abort_if_args_present <- function(.full) {
  if (!has_length(call_args(.full), 0)) {
    abort("`.full` expects `start()`/`end()` with no arguments.")
  }
}
