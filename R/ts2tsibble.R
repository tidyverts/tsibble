#' @rdname as-tsibble
#' @param tz Time zone. May be useful when a `ts` object is more frequent than
#' daily.
#'
#' @examples
#' # coerce ts to tsibble
#' as_tsibble(AirPassengers)
#' as_tsibble(sunspot.year)
#' as_tsibble(sunspot.month)
#' as_tsibble(austres)
#' @export
as_tsibble.ts <- function(x, ..., tz = "UTC") {
  idx <- time_to_date(x, tz = tz)
  value <- as.numeric(x) # rm its ts class
  tbl <- tibble(index = idx, value = value)
  build_tsibble(tbl,
    key = NULL, index = index, ordered = TRUE, validate = FALSE
  )
}

#' @rdname as-tsibble
#' @param pivot_longer `TRUE` gives a "longer" form of the data, otherwise as is.
#' @param gather \lifecycle{defunct} Please use `pivot_longer` instead.
#'
#' @examples
#' # coerce mts to tsibble
#' z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
#' as_tsibble(z)
#' as_tsibble(z, pivot_longer = FALSE)
#' @export
as_tsibble.mts <- function(x, ..., tz = "UTC", pivot_longer = TRUE, 
                           gather = deprecated()) {
  if (!is_missing(gather)) {
    lifecycle::deprecate_stop("0.8.0", 
      "as_tsibble(gather = )", "as_tsibble(pivot_longer = )")
  }
  if (pivot_longer) {
    long_tbl <- pivot_longer_tsibble(x, tz = tz)
    build_tsibble(
      long_tbl,
      key = key, index = index, ordered = TRUE, validate = FALSE
    )
  } else {
    wide_tbl <- make_index_explicit(x, tz = tz)
    build_tsibble(
      wide_tbl,
      key = NULL, index = index, ordered = TRUE, validate = FALSE
    )
  }
}

make_index_explicit <- function(x, tz = "UTC") {
  vec_cbind(index = time_to_date(x, tz = tz), as_tibble(x))
}

pivot_longer_tsibble <- function(x, tz = "UTC") {
  idx <- time_to_date(x, tz = tz)
  list2(
    "index" := vec_repeat(idx, times = NCOL(x)), 
    "key" := vec_repeat(colnames(x), each = vec_size(x)),
    "value" := vec_c(!!!unclass(x))
  )
}

# from ts time to dates
time_to_date <- function(x, tz = "UTC", ...) {
  freq <- frequency(x)
  time_x <- round(as.numeric(time(x)), digits = 6) # floating
  if (freq == 52) {
    warn("Expected frequency of weekly data: 365.25 / 7 (approx 52.18), not  52.")
  }
  if (freq == 7) { # daily
    start_year <- trunc(time_x[1])
    as.Date(round_date(
      date_decimal(start_year + (time_x - start_year) * 7 / 365),
      unit = "day"
    ))
  } else if (round(freq, 2) == 52.18) { # weekly
    yearweek(date_decimal(time_x))
  } else if (freq > 4 && freq <= 12) { # monthly
    yearmonth(time_x)
  } else if (freq > 1 && freq <= 4) { # quarterly
    yearquarter(time_x)
  } else if (freq == 1) { # yearly
    time_x
  } else {
    if (end(x)[1] > 1581) {
      date_x <- date_decimal(time_x, tz = tz)
      round_date(date_x, unit = "seconds")
    } else {
      time_x
    }
  }
}

# nocov start

#' @keywords internal
#' @export
as_tsibble.msts <- function(x, ..., tz = "UTC", pivot_longer = TRUE) {
  if (NCOL(x) == 1) {
    as_tsibble.ts(x, ..., tz = tz)
  } else {
    as_tsibble.mts(x, ..., tz = tz, pivot_longer = pivot_longer)
  }
}

#' @rdname as-tsibble
#' @usage NULL
#' @export
as_tsibble.hts <- function(x, ..., tz = "UTC") {
  full_labs <- extract_labels(x)
  tbl <- pivot_longer_tsibble(x$bts, tz = tz)[c("index", "value")]
  tbl_hts <- vec_cbind(!!!full_labs, !!!tbl)
  # this would work around the special character issue in headers for parse()
  key <- colnames(tbl_hts)[1:vec_size(full_labs)]
  build_tsibble(tbl_hts,
    key = !!key, index = index, ordered = TRUE,
    validate = FALSE
  )
}

# recursive function to repeat nodes for hts
rep_nodes <- function(x, level = 1L, index = seq_along(x[[level]])) {
  if (has_length(x[[1]], 1)) {
    x <- x[-1]
  }
  index <- rep.int(index, x[[level]])
  if (has_length(x, level)) {
    index
  } else {
    rep_nodes(x, level + 1L, index)
  }
}

extract_labels <- function(x) {
  nodes <- x$nodes
  old_labels <- x$labels
  btm_labels <- old_labels[[length(old_labels)]]
  new_labels <- old_labels[-c(1, length(old_labels))]
  chr_labs <- map2(
    new_labels, seq_along(new_labels), ~ .x[rep_nodes(nodes, level = .y)]
  )
  nr <- nrow(x$bts)
  full_labs <- map(chr_labs, ~ rep(., each = nr))
  full_labs <- c(full_labs, list(rep(btm_labels, each = nr)))
  names(full_labs) <- names(old_labels[-1])
  full_labs
}
# nocov end
