#' Lagged differences
#'
#' @description
#' \lifecycle{stable}
#'
#' @inheritParams dplyr::lag
#' @param lag A positive integer indicating which lag to use.
#' @param differences A positive integer indicating the order of the difference.
#' @param relative A logical indicating whether the result is a relative difference or not.
#'
#' @return A numeric vector of the same length as `x`.
#' @seealso [keyed_lag], [keyed_lead], [keyed_difference], [dplyr::lead] and [dplyr::lag]
#' @export
#' @examples
#' # examples from base
#' difference(1:10, 2)
#' difference(1:10, 2, 2)
#' x <- cumsum(cumsum(1:10))
#' difference(x, lag = 2)
#' difference(x, differences = 2)
#' difference(x, relative = TRUE)
#' # Use order_by if data not already ordered (example from dplyr)
#' library(dplyr, warn.conflicts = FALSE)
#' tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)
#' scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#'
#' wrong <- mutate(scrambled, diff = difference(value))
#' arrange(wrong, year)
#'
#' right <- mutate(scrambled, diff = difference(value, order_by = year))
#' arrange(right, year)
difference <- function(x, lag = 1L, differences = 1L, default = NA,
                       order_by = NULL, relative = FALSE) {
    if (lag < 1L || differences < 1L) {
        abort("`lag` and `differences` must be positive integers.")
    }
    if (relative == TRUE && any(x == 0)) {
        abort("Relative change is not defined when the reference value is 0.")
    }
    if (relative == TRUE && differences > 1L) {
        warning("Relative `differences` greater than 1 are relative differences of relative differences")
    }
    if (lag * differences >= length(x)) {
        abort("Number of observations must exceed `lag * differences`.")
    }
    if (is_null(order_by)) {
        diff_impl(x, lag = lag, differences = differences, default = default, relative = relative)
    } else {
        with_order(order_by, diff_impl, x,
                   lag = lag, differences = differences, default = default, relative = relative
        )
    }
}

diff_impl <- function(x, lag = 1L, differences = 1L, default = NA, relative = FALSE) {
    i1 <- -seq_len(lag)
    if (relative == FALSE) {
        for (i in seq_len(differences)) {
            x <- x[i1] - x[-length(x):-(length(x)-lag+1L)]
        }
    } else {
        for (i in seq_len(differences)) {
            x <- (x[i1] - x[-length(x):-(length(x)-lag+1L)]) / abs(x[1L:(length(x)-lag)])
        }
    }
    vec_c(vec_repeat(default, lag * differences), x)
}

lag_lead <- function(x, n = 1L, default = NA, order_by, op = c("lead", "lag")) {
  if (n < 0 || !is_integerish(n, n = 1)) {
    abort("`n` must be a non-negative integer.")
  }
  op <- arg_match(op)
  if (op == "lead") {
    idx <- vec_match(order_by + n, order_by) # tlead()
  } else {
    idx <- vec_match(order_by - n, order_by) # tlag()
  }
  x <- vec_slice(x, idx)
  idx_na <- vec_in(idx, NA)
  vec_slice(x, idx_na) <- default
  x
}

tlag <- function(x, n = 1L, default = NA, order_by) {
  lag_lead(x, n, default, order_by, "lag")
}

tlead <- function(x, n = 1L, default = NA, order_by) {
  lag_lead(x, n, default, order_by, "lead")
}

tdifference <- function(x, lag = 1, differences = 1, default = NA, order_by) {
  if (lag < 1 || differences < 1) {
    abort("`lag` and `differences` must be positive integers.")
  }
  idx <- vec_match(order_by - lag, order_by)
  for (i in seq_len(differences)) {
    x <- vec_slice(x, idx) - x
  }
  idx_na <- vec_in(idx, NA)
  vec_slice(x, idx_na) <- default
  x
}

#' Tsibble-aware lag, lead, and difference operations
#'
#' They are specialist vector functions, used in tsibble data context inside
#' `mutate()`/`transmute()`. Unlike their counterparts, such as `lag()` and `lead()`,
#' `keyed_*()` *take care of* temporal ordering and gaps, and do the right thing.
#' They ignore the grouping data structure.
#'
#' @param select Unquoted variable.
#' @inheritParams dplyr::lag 
#' @inheritParams difference
#'
#' @rdname keyed-vec
#' @export
#' @examples
#' library(dplyr)
#' yrmth <- c("2018-07", "2018-08", "2018-11", "2018-12", "2019-01", "2019-03")
#' tsbl <- tsibble(
#'   yrmth = yearmonth(yrmth), 
#'   income = c(1153, 1181, 1236, 1297, 1264, 1282),
#'   index = yrmth)
#' 
#' tsbl %>% 
#'   mutate(
#'     lag_income = keyed_lag(income),
#'     lead_income = keyed_lead(income),
#'     diff_income = keyed_difference(income),
#'   )
#'
#' # take care of key and gaps
#' pedestrian %>% 
#'   mutate(l_count = keyed_lag(Count))
#' 
#' # to replace the following chain
#' pedestrian %>% 
#'   fill_gaps() %>% 
#'   group_by_key() %>% 
#'   mutate(l_count = lag(Count))
keyed_lag <- function(select, n = 1L, default = NA) {
  mask <- peek_tsibble_mask()
  data <- mask$retrieve_data()
  abort_if_irregular(data)
  tunits <- default_time_units(interval(data))
  mask$map_keyed_chunks(!!enquo(select), data = data, 
    tlag, n = n * tunits, default, index(data))
}

#' @rdname keyed-vec
#' @export
keyed_lead <- function(select, n = 1L, default = NA) {
  mask <- peek_tsibble_mask()
  data <- mask$retrieve_data()
  abort_if_irregular(data)
  tunits <- default_time_units(interval(data))
  mask$map_keyed_chunks(!!enquo(select), data = data, 
    tlead, n = n * tunits, default, index(data))
}

#' @rdname keyed-vec
#' @export
keyed_difference <- function(select, lag = 1L, differences = 1L, default = NA, relative = FALSE) {
  mask <- peek_tsibble_mask()
  data <- mask$retrieve_data()
  abort_if_irregular(data)
  tunits <- default_time_units(interval(data))
  mask$map_keyed_chunks(!!enquo(select), data = data, 
    tdifference, lag = lag * tunits, differences, default, relative, index(data))
}
