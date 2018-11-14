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
#' @family implict gaps handling
#' @seealso [tidyr::fill], [tidyr::replace_na]
#' @rdname fill-na
#' @export
fill_na <- function(.data, ...) {
  if (NROW(.data) == 0L || NROW(.data) == 1L) return(.data)
    
  UseMethod("fill_na")
}

#' @export
fill_na.data.frame <- function(.data, ...) {
  abort("Do you need `tidyr::complete()` for a `tbl_df`/`data.frame`?")
}

#' @rdname fill-na
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
#' fill_na(harvest, .full = TRUE)
#' full_harvest <- fill_na(harvest, .full = FALSE)
#' full_harvest
#'
#' # use fill() to fill `NA` by previous/next entry
#' full_harvest %>% 
#'   group_by(fruit) %>% 
#'   tidyr::fill(kilo, .direction = "down")
#'
#' # replace gaps with a specific value ----
#' harvest %>%
#'   fill_na(kilo = 0L)
#'
#' # replace gaps using a function by variable ----
#' harvest %>%
#'   fill_na(kilo = sum(kilo))
#'
#' # replace gaps using a function for each group ----
#' harvest %>%
#'   group_by(fruit) %>%
#'   fill_na(kilo = sum(kilo))
#'
#' # leaves existing `NA` untouched ----
#' harvest[2, 3] <- NA
#' harvest %>%
#'   group_by(fruit) %>%
#'   fill_na(kilo = sum(kilo, na.rm = TRUE))
#'
#' # replace NA ----
#' pedestrian %>%
#'   group_by(Sensor) %>%
#'   fill_na(Count = as.integer(median(Count)))
#' @export
fill_na.tbl_ts <- function(.data, ..., .full = FALSE) {
  not_regular(.data)
  int <- interval(.data)
  idx <- index(.data)
  idx_chr <- as_string(idx)
  key <- key(.data)
  keyed_tbl <- grped_df_by_key(.data)
  if (.full) {
    idx_full <- seq_generator(eval_tidy(idx, data = keyed_tbl), int)
    ref_data <- keyed_tbl %>% 
      summarise(!! idx_chr := list(tibble(!! idx_chr := idx_full))) %>% 
      tidyr::unnest(!! idx) %>% 
      ungroup()
  } else {
    ref_data <- keyed_tbl %>% 
      summarise(
        !! idx_chr := list(tibble(!! idx_chr := seq_generator(!! idx, int)))
      ) %>% 
      unnest(!! idx) %>% 
      ungroup()
  }

  cn <- names(.data)
  lst_exprs <- enquos(..., .named = TRUE)
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

seq_generator <- function(x, interval = NULL) {
  min_x <- min0(x)
  max_x <- max0(x)
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
  tryCatch(
    min_x + seq.int(0, as.double(max_x - min_x), tunit),
    error = function(e) {
      e$call <- NULL
      e$message <- sprintf("Neither `+` nor `seq()` are defined for class %s", class(x)[1L])
      stop(e)
    }
  )
}
