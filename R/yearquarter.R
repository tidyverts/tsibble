#' Represent year-quarter
#'
#' @description
#' \lifecycle{stable}
#'
#' Create or coerce using `yearquarter()`.
#'
#' @inheritSection yearmonth Display
#'
#' @inheritParams yearmonth
#'
#' @return year-quarter (`yearquarter`) objects.
#'
#' @family index functions
#' @rdname year-quarter
#' @export
#' @examples
#' # coerce POSIXct/Dates to yearquarter
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 quarter")
#' yearquarter(x)
#'
#' # parse characters
#' yearquarter(c("2018 Q1", "2018 Qtr1", "2018 Quarter 1"))
#'
#' # creat an empty yearquarter container
#' yearquarter()
#'
#' # seq() and arithmetic
#' qtr <- yearquarter("2017 Q1")
#' seq(qtr, length.out = 10, by = 1) # by 1 quarter
#' qtr + 0:9
#'
#' # display formats
#' format(qtr, format = "%y Qtr%q")
#'
#' # units since 1970 Q1
#' as.double(yearquarter("1969 Q1") + 0:8)
yearquarter <- function(x) {
  UseMethod("yearquarter")
}

#' @export
yearquarter.default <- function(x) {
  dont_know(x, "yearquarter")
}

#' @export
yearquarter.NULL <- function(x) {
  new_yearquarter()
}

#' @export
yearquarter.POSIXct <- function(x) {
  new_yearquarter(floor_date(as_date(x), unit = "quarters"))
}

#' @export
yearquarter.POSIXlt <- yearquarter.POSIXct

#' @export
yearquarter.Date <- yearquarter.POSIXct

#' @export
yearquarter.character <- function(x) {
  # exact matching with q, qtr, or quarter
  key_words <- regmatches(x, gregexpr("[[:alpha:]]+", x))
  if (all(grepl("^(q|qtr|quarter)$", key_words, ignore.case = TRUE))) {
    yr_qtr <- regmatches(x, gregexpr("[[:digit:]]+", x))
    digits_lgl <- map_lgl(yr_qtr, ~ !has_length(.x, 2))
    digits_len <- map_int(yr_qtr, ~ sum(nchar(.x)))
    if (any(digits_lgl) || any(digits_len != 5)) {
      abort("Character strings are not in a standard unambiguous format.")
    }
    yr_lgl <- map(yr_qtr, ~ grepl("[[:digit:]]{4}", .x))
    yr <- as.integer(map2_chr(yr_qtr, yr_lgl, ~ .x[.y]))
    qtr <- as.integer(map2_chr(yr_qtr, yr_lgl, ~ .x[!.y]))
    if (any(qtr > 4)) {
      abort("Quarters can't be greater than 4.")
    }
    new_yearquarter(make_date(yr, qtr * 3 - 2))
  } else {
    assertDate(x)
    new_yearquarter(anydate(x))
  }
}

#' @export
yearquarter.yearweek <- yearquarter.POSIXct

#' @export
yearquarter.yearmonth <- yearquarter.POSIXct

#' @export
yearquarter.yearquarter <- function(x) {
  x
}

#' @export
yearquarter.numeric <- function(x) {
  new_yearquarter(0) + x
}

#' @export
yearquarter.yearqtr <- function(x) {
  year <- trunc(x)
  last_month <- trunc((x %% 1) * 4 + 1) * 3
  first_month <- formatC(last_month - 2, flag = 0, width = 2)
  result <- make_date(year, first_month, 1)
  new_yearquarter(result)
}

new_yearquarter <- function(x = double()) {
  new_vctr(x, class = "yearquarter")
}

#' @rdname year-quarter
#' @export
is_yearquarter <- function(x) {
  inherits(x, "yearquarter")
}

#' @export
is.numeric.yearquarter <- function(x) {
  FALSE
}

#' @export
tz.yearquarter <- function(x) {
  "UTC"
}

# diff.yearquarter <- function(x, lag = 1, differences = 1, ...) {
#   out <- diff((year(x) - 1970) * 4 + quarter(x),
#     lag = lag, differences = differences
#   )
#   structure(out, class = "difftime", units = "quarters")
# }

#' @rdname tsibble-vctrs
#' @export
vec_cast.yearquarter <- function(x, to, ...) {
  UseMethod("vec_cast.yearquarter")
}

#' @export
vec_cast.Date.yearquarter <- function(x, to, ...) {
  new_date(x)
}

#' @export
vec_cast.POSIXct.yearquarter <- function(x, to, ...) {
  as.POSIXct(new_date(x), ...)
}

#' @export
vec_cast.double.yearquarter <- function(x, to, ...) {
  as.double((year(x) - 1970) * 4 + quarter(x) - 1)
}

#' @export
vec_cast.POSIXlt.yearquarter <- function(x, to, ...) {
  as.POSIXlt(new_date(x), ...)
}

#' @export
vec_cast.yearquarter.yearquarter <- function(x, to, ...) {
  new_yearquarter(x)
}

#' @export
vec_cast.character.yearquarter <- function(x, to, ...) {
  format(x)
}

#' @rdname tsibble-vctrs
#' @export
vec_ptype2.yearquarter <- function(x, y, ...) {
  UseMethod("vec_ptype2.yearquarter", y)
}

#' @export
vec_ptype2.yearquarter.POSIXct <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.POSIXct.yearquarter <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.yearquarter.Date <- function(x, y, ...) {
  new_date()
}

#' @export
vec_ptype2.yearquarter.yearquarter <- function(x, y, ...) {
  new_yearquarter()
}

#' @export
vec_ptype2.Date.yearquarter <- function(x, y, ...) {
  new_date()
}

#' @rdname tsibble-vctrs
#' @method vec_arith yearquarter
#' @export
vec_arith.yearquarter <- function(op, x, y, ...) {
  UseMethod("vec_arith.yearquarter", y)
}

#' @method vec_arith.yearquarter default
#' @export
vec_arith.yearquarter.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.yearquarter numeric
#' @export
vec_arith.yearquarter.numeric <- function(op, x, y, ...) {
  if (op == "+") {
    new_yearquarter(as_date(x) + period(months = y * 3))
  } else if (op == "-") {
    new_yearquarter(as_date(x) - period(months = y * 3))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yearquarter yearquarter
#' @export
vec_arith.yearquarter.yearquarter <- function(op, x, y, ...) {
  if (op == "-") {
    as.double(x) - as.double(y)
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.numeric yearquarter
#' @export
vec_arith.numeric.yearquarter <- function(op, x, y, ...) {
  if (op == "+") {
    yearquarter(period(months = x * 3) + as_date(y))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yearquarter MISSING
#' @export
vec_arith.yearquarter.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = x,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @export
format.yearquarter <- function(x, format = "%Y Q%q", ...) {
  x <- as_date(x)
  qtr <- quarter(x)
  qtr_sub <- map_chr(qtr, function(z) gsub("%q", z, x = format))
  qtr_sub[is.na(qtr_sub)] <- "-" # NA formats cause errors

  format.Date(x, format = qtr_sub)
}

#' @rdname tsibble-vctrs
#' @export
obj_print_data.yearquarter <- function(x, ...) {
  if (length(x) == 0) return()
  print(format(x))
}

#' @export
vec_ptype_abbr.yearquarter <- function(x, ...) {
  "qtr"
}

#' @export
seq.yearquarter <- function(from, to, by, length.out = NULL, along.with = NULL,
                            ...) {
  bad_by(by)
  by_mth <- paste(by, "quarter")
  from <- vec_cast(from, new_date())
  if (!is_missing(to)) {
    to <- vec_cast(to, new_date())
  }
  new_yearquarter(seq_date(
    from = from, to = to, by = by_mth, length.out = length.out,
    along.with = along.with, ...
  ))
}

seq.ordered <- function(from, to, by, ...) {
  bad_by(by)
  lvls <- levels(from)
  idx_from <- which(lvls %in% from)
  idx_to <- which(lvls %in% to)
  idx <- seq.int(idx_from, idx_to, by = by)
  ordered(lvls[idx], levels = lvls)
}
