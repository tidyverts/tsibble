globalVariables(".")

#' Turn implicit missing values into explicit missing values
#'
#' @param .data A tsibble.
#' @param ... A set of name-value pairs. The values provided will only replace 
#' missing values that were marked as "implicit", and will leave previously
#' existing `NA` untouched.
#' * empty: filled with default `NA`.
#' * filled by values or functions.
#' @param .full `FALSE` to insert `NA` for each series within its own period. `TRUE`
#' to fill `NA` over the entire time span of the data (a.k.a. fully balanced panel).
#'
#' @family implicit gaps handling
#' @seealso [tidyr::fill], [tidyr::replace_na] for handling missing values `NA`.
#' @export
#' @examples
#' harvest <- tsibble(
#'   year = c(2010, 2011, 2013, 2011, 2012, 2014),
#'   fruit = rep(c("kiwi", "cherry"), each = 3),
#'   kilo = sample(1:10, size = 6),
#'   key = id(fruit), index = year
#' )
#'
#' # gaps as default `NA` ----
#' fill_gaps(harvest, .full = TRUE)
#' full_harvest <- fill_gaps(harvest, .full = FALSE)
#' full_harvest
#'
#' # use fill() to fill `NA` by previous/next entry
#' full_harvest %>% 
#'   group_by(fruit) %>% 
#'   fill(kilo, .direction = "down")
#'
#' # replace gaps with a specific value ----
#' harvest %>%
#'   fill_gaps(kilo = 0L)
#'
#' # replace gaps using a function by variable ----
#' harvest %>%
#'   fill_gaps(kilo = sum(kilo))
#'
#' # replace gaps using a function for each group ----
#' harvest %>%
#'   group_by(fruit) %>%
#'   fill_gaps(kilo = sum(kilo))
#'
#' # leaves existing `NA` untouched ----
#' harvest[2, 3] <- NA
#' harvest %>%
#'   group_by(fruit) %>%
#'   fill_gaps(kilo = sum(kilo, na.rm = TRUE))
#'
#' # replace NA ----
#' pedestrian %>%
#'   group_by(Sensor) %>%
#'   fill_gaps(Count = as.integer(median(Count)))
fill_gaps <- function(.data, ..., .full = FALSE) {
  UseMethod("fill_gaps")
}

#' @export
fill_gaps.data.frame <- function(.data, ...) {
  abort("Do you need `tidyr::complete()` for a `tbl_df`/`data.frame`?")
}

#' @export
fill_gaps.tbl_ts <- function(.data, ..., .full = FALSE) {
  if (NROW(.data) == 0L || NROW(.data) == 1L) return(.data)

  gap_data <- scan_gaps(.data, .full = .full)
  lst_exprs <- enquos(..., .named = TRUE)
  if (NROW(gap_data) == 0) {
    if (!is_empty(lst_exprs)) {
      warn("`.data` is a complete tsibble. Values passed to `...` are ignored.")
    }
    return(.data)
  }

  cn <- names(.data)
  if (!is_empty(lst_exprs)) { # any replacement
    # error handling
    tidyselect::vars_select(measured_vars(.data), !!! names(lst_exprs))
    replaced_df <- ungroup(summarise(as_grouped_df(.data), !!! lst_exprs))
    by_name <- intersect(names(gap_data), names(replaced_df))

    if (NROW(replaced_df) > NROW(gap_data)) {
      abort(sprintf(
        "Replacement has length %s, not 1 or %s.", 
        NROW(replaced_df), NROW(gap_data)
      ))
    } else if (is_empty(by_name)) { # by value
      gap_data <- mutate(gap_data, !!! replaced_df)
    } else { # by function
      gap_data <- left_join(gap_data, replaced_df, by = by_name)
    }
  }
  full_data <- dplyr::bind_rows(as_tibble(gap_data), .data)
  if (!identical(cn, names(full_data))) {
    full_data <- select(full_data, !!! syms(cn)) # keep the original order
  }
  update_tsibble(full_data, .data, ordered = NULL, interval = interval(.data))
}

#' Scan a tsibble for implicit missing observations
#'
#' @inheritParams count_gaps
#' @family implicit gaps handling
#' @export
#' @examples
#' scan_gaps(pedestrian)
scan_gaps <- function(.data, .full = FALSE, ...) {
  UseMethod("scan_gaps")
}

#' @export
scan_gaps.tbl_ts <- function(.data, .full = FALSE, ...) {
  not_regular(.data)
  int <- interval(.data)
  idx <- index(.data)
  idx_chr <- as_string(idx)
  if (unknown_interval(int)) return(.data[0L, c(key_vars(.data), idx_chr)])

  key <- key(.data)
  keyed_tbl <- as_grouped_df(group_by_key(.data))
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = keyed_tbl), int)
    sum_data <- 
      summarise(
        keyed_tbl, 
        !! idx_chr := list2(!! idx_chr := idx_full)
      )
  } else {
    sum_data <- 
      summarise(
        keyed_tbl,
        !! idx_chr := list2(!! idx_chr := seq_generator(!! idx, int))
      )
  }
  ref_data <- ungroup(unnest(sum_data, !! idx))
  if (NROW(ref_data) == NROW(.data)) {
    return(.data[0L, c(key_vars(.data), idx_chr)])
  }

  gap_data <- anti_join(ref_data, .data, by = c(key_vars(.data), idx_chr))
  update_tsibble(gap_data, .data, ordered = NULL, interval = interval(.data))
}

#' Count implicit gaps
#' 
#' @param .data A `tbl_ts`.
#' @param .full `FALSE` to find gaps for each series within its own period. 
#' `TRUE` to find gaps over the entire time span of the data.
#' @param ... Other arguments passed on to individual methods.
#'
#' @family implicit gaps handling
#' @export
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * ".from": the starting time point of the gap
#' * ".to": the ending time point of the gap
#' * ".n": the number of implicit missing observations during the time period
count_gaps <- function(.data, ...) {
  UseMethod("count_gaps")
}

#' @export
#' @examples
#' ped_gaps <- pedestrian %>% 
#'   count_gaps(.full = TRUE)
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
count_gaps <- function(.data, .full = FALSE, ...) {
  UseMethod("count_gaps")
}

#' @export
count_gaps.tbl_ts <- function(.data, .full = FALSE, ...) {
  not_regular(.data)
  int <- interval(.data)
  idx <- index(.data)

  gap_data <- scan_gaps(.data, .full = .full, ...)
  if (unknown_interval(int) || NROW(gap_data) == 0L) {
    data_key <- .data[0L, key_vars(.data)] 
    data_key[[".to"]] <- data_key[[".from"]] <- .data[[as_string(idx)]][0L]
    data_key[[".n"]] <- integer()
    return(data_key)
  }

  idx_full <- seq_generator(eval_tidy(idx, data = gap_data), int)
  grped_tbl <- as_grouped_df(group_by_key(gap_data))
  lst_out <- 
    summarise(
      grped_tbl, 
      !! ".gaps" := list2(tbl_gaps(unique(!! idx), idx_full))
    )

  idx_type <- class(lst_out[[".gaps"]][[1]][[".from"]])
  out <- unnest(lst_out, .gaps)
  class(out[[".from"]]) <- class(out[[".to"]]) <- idx_type
  tibble(!!! out)
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
#'   key = id(fruit), index = year
#' )
#' has_gaps(harvest)
#' has_gaps(harvest, .full = TRUE)
has_gaps <- function(.data, .full = FALSE, ...) {
  UseMethod("has_gaps")
}

#' @export
has_gaps.tbl_ts <- function(.data, .full = FALSE, ...) {
  if (NROW(.data) == 0L) return(tibble(!! ".gaps" := FALSE))

  not_regular(.data)
  int <- interval(.data)
  idx <- index(.data)
  grped_tbl <- as_grouped_df(group_by_key(.data))
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = .data), int)
    res <- 
      summarise(
        grped_tbl,
        !! ".gaps" := (length(idx_full) - length(!! idx)) > 0
      )
  } else {
    res <- 
      summarise(
        grped_tbl,
        !! ".gaps" := (length(seq_generator(!! idx, int)) - length(!! idx)) > 0
      )
  }
  tibble(!!! res)
}

tbl_gaps <- function(x, y) {
  len_x <- length(x)
  len_y <- length(y)
  if (len_y < len_x) {
    msg <- sprintf(
      "`length(x)` (%d) must not be greater than `length(y)` (%d).",
      len_x, len_y
    )
    abort(msg)
  }
  gap_vec <- logical(length = len_y)
  gap_vec[match(x, y)] <- TRUE
  gap_rle <- rle(gap_vec)
  lgl_rle <- as.logical(gap_rle$values)
  gap_idx <- gap_rle$lengths
  to <- cumsum(gap_idx)
  from <- c(1, to[-length(to)] + 1)
  nobs <- gap_idx[lgl_rle]
  if (is_empty(nobs)) {
    tibble(.from = y[0], .to = y[0], .n = integer())
  } else {
    tibble(
      .from = y[from][lgl_rle],
      .to = y[to][lgl_rle],
      .n = nobs
    )
  }
}

seq_generator <- function(x, interval = NULL) {
  if (is_empty(x)) return(x)

  min_x <- min(x)
  max_x <- max(x)
  if (is_null(interval)) {
    interval <- pull_interval(x)
  }
  tunit <- time_unit(interval)
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
    res2 <- hms::as.hms(res2)
  }
  res2
}
