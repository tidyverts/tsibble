#' Count implicit gaps
#' 
#' @param .data A `tbl_ts`.
#' @param ... Other arguments passed on to individual methods.
#'
#' @rdname count-gaps
#' @export
#' @seealso [fill_na]
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
      summarise(gaps = list(gaps(!! idx, idx_full))) %>% 
      unnest(gaps)
  } else {
    out <- grped_tbl %>% 
      summarise(gaps = list(gaps(!! idx, seq_generator(!! idx, int)))) %>% 
      unnest(gaps)
  }
  ungroup(out)
}

#' Does a tsibble have implicit gaps in time?
#'
#' @inheritParams count_gaps
#' @export
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
  ungroup(res)
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
  gap_rle <- rle_lgl(gap_vec)
  lgl_rle <- gap_rle$values
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

