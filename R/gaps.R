globalVariables(".")

#' Turn implicit missing values into explicit missing values
#'
#' @param .data A tsibble.
#' @param ... A set of name-value pairs. The values provided will only replace 
#' missing values that were marked as "implicit", and will leave previously
#' existing `NA` untouched.
#' * empty: filled with default `NA`.
#' * filled by values or functions.
#'
#' @family implicit gaps handling
#' @seealso [tidyr::fill], [tidyr::replace_na] for handling missing values `NA`.
#' @rdname fill-gaps
#' @export
fill_gaps <- function(.data, ...) {
  if (NROW(.data) == 0L || NROW(.data) == 1L) return(.data)
  UseMethod("fill_gaps")
}

#' @export
fill_gaps.data.frame <- function(.data, ...) {
  abort("Do you need `tidyr::complete()` for a `tbl_df`/`data.frame`?")
}

#' @rdname fill-gaps
#' @param .full `FALSE` to insert `NA` for each key within its own period. `TRUE`
#' to fill `NA` over the entire time span of the data (a.k.a. fully balanced panel).
#'
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
#' @export
fill_gaps.tbl_ts <- function(.data, ..., .full = FALSE) {
  not_regular(.data)
  int <- interval(.data)
  idx <- index(.data)
  idx_chr <- as_string(idx)
  key <- key(.data)
  keyed_tbl <- grped_df_by_key(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = keyed_tbl), int)
    sum_data <- keyed_tbl %>% 
      summarise(!! idx_chr := list2(!! idx_chr := idx_full))
  } else {
    sum_data <- keyed_tbl %>% 
      summarise(
        !! idx_chr := list2(!! idx_chr := seq_generator(!! idx, int))
      )
  }
  ref_data <- ungroup(unnest(sum_data, !! idx))

  lst_exprs <- enquos(..., .named = TRUE)
  if (NROW(ref_data) == NROW(.data)) {
    if (!is_empty(lst_exprs)) {
      warn("`.data` is a complete tsibble. Values passed to `...` are ignored.")
    }
    return(.data)
  }

  cn <- names(.data)
  if (is_empty(lst_exprs)) { # no replacement
    ord <- TRUE
    full_data <- ref_data %>% 
      left_join(.data, by = c(key_vars(.data), idx_chr))
  } else {
    lhs <- names(lst_exprs)
    check_names <- lhs %in% cn
    if (is_false(all(check_names))) {
      bad_names <- paste_comma(lhs[which(!check_names)])
      abort(sprintf("Can't find column `%s` in `.data`.", bad_names))
    }

    ord <- NULL
    replaced_df <- as_grouped_df(.data) %>% 
      summarise(!!! lst_exprs) %>% 
      ungroup()
    filled_data <- ref_data %>% 
      anti_join(.data, by = c(key_vars(.data), idx_chr))
    by_name <- intersect(names(filled_data), names(replaced_df))

    if (NROW(replaced_df) > NROW(filled_data)) {
      abort(sprintf(
        "Replacement has length %s, not 1 or %s.", 
        NROW(replaced_df), NROW(filled_data)
      ))
    } else if (is_empty(by_name)) { # by value
      filled_data <- filled_data %>% 
        mutate(!!! replaced_df)
    } else { # by function
      filled_data <- filled_data %>% 
        left_join(replaced_df, by = by_name)
    }
    full_data <- filled_data %>% 
      dplyr::bind_rows(.data)
  }
  if (!identical(cn, names(full_data))) {
    full_data <- full_data %>%
      select(!!! syms(cn)) # keep the original order
  }
  update_tsibble(full_data, .data, ordered = ord, interval = interval(.data))
}

#' Count implicit gaps
#' 
#' @param .data A `tbl_ts`.
#' @param ... Other arguments passed on to individual methods.
#'
#' @family implicit gaps handling
#' @rdname count-gaps
#' @export
#' @return
#' A tibble contains:
#' * the "key" of the `tbl_ts`
#' * ".from": the starting time point of the gap
#' * ".to": the ending time point of the gap
#' * ".n": the implicit missing observations during the time period
count_gaps <- function(.data, ...) {
  UseMethod("count_gaps")
}

#' @rdname count-gaps
#' @param .full `FALSE` to find gaps for each group within its own period. `TRUE`
#' to find gaps over the entire time span of the data.
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
count_gaps.tbl_ts <- function(.data, .full = FALSE, ...) {
  not_regular(.data)
  int <- interval(.data)

  idx <- index(.data)
  grped_tbl <- grped_df_by_key(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = .data), int)
    out <- grped_tbl %>% 
      summarise(gaps = list2(gaps(!! idx, idx_full))) %>% 
      unnest(gaps)
  } else {
    out <- grped_tbl %>% 
      summarise(gaps = list2(gaps(!! idx, seq_generator(!! idx, int)))) %>% 
      unnest(gaps)
  }
  tibble(!!! out)
}

#' Does a tsibble have implicit gaps in time?
#'
#' @inheritParams count_gaps
#' @export
#' @family implicit gaps handling
#' @rdname has-gaps
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
has_gaps <- function(.data, ...) {
  if (NROW(.data) == 0L) return(tibble(!! ".gaps" := FALSE))
    
  UseMethod("has_gaps")
}

#' @rdname has-gaps
#' @export
has_gaps.tbl_ts <- function(.data, .full = FALSE, ...) {
  not_regular(.data)
  int <- interval(.data)
  idx <- index(.data)
  grped_tbl <- grped_df_by_key(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = .data), int)
    res <- grped_tbl %>% 
      summarise(!! ".gaps" := (length(idx_full) - length(!! idx)) > 0)
  } else {
    res <- grped_tbl %>% 
      summarise(
        !! ".gaps" := (length(seq_generator(!! idx, int)) - length(!! idx)) > 0
      )
  }
  tibble(!!! res)
}

#' Find missing elements in `x` with respect to `y`
#'
#' @param x,y Atomic vectors. The length of `y` must be greater than the length of `x`.
#' @return A tibble of columns `.from`, `.to` and `.n`.
#' @keywords internal
#' @export
#' @examples
#' gaps(x = c(1:3, 5:6, 9:10), y = 1:10)
gaps <- function(x, y) {
  if (is_empty(x) && is_empty(y)) {
    return(tibble(.from = x, .to = y, .n = integer()))
  }
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
  gap_vec[-match(x, y)] <- TRUE
  gap_rle <- rle(gap_vec)
  lgl_rle <- as.logical(gap_rle$values)
  gap_idx <- gap_rle$lengths
  to <- cumsum(gap_idx)
  from <- c(1, to[-length(to)] + 1)
  nobs <- gap_idx[lgl_rle]
  if (is_empty(nobs)) {
    tibble(.from = NA, .to = NA, .n = 0L)
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
