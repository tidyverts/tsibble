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
#' @inheritParams lubridate::quarter
#'
#' @return year-quarter (`yearquarter`) objects.
#'
#' @seealso [scale_x_yearquarter] and others for ggplot2 scales
#' @family index functions
#' @rdname year-quarter
#' @export
#' @examples
#' # coerce POSIXct/Dates to yearquarter
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 quarter")
#' yearquarter(x)
#' yearquarter(x, fiscal_start = 6)
#'
#' # parse characters
#' yearquarter(c("2018 Q1", "2018 Qtr1", "2018 Quarter 1"))
#'
#' # seq() and arithmetic
#' qtr <- yearquarter("2017 Q1")
#' seq(qtr, length.out = 10, by = 1) # by 1 quarter
#' qtr + 0:9
#'
#' # display formats
#' format(qtr, format = "%y Qtr%q")
yearquarter <- function(x, fiscal_start = 1) {
  UseMethod("yearquarter")
}

#' @rdname year-quarter
#' @param year,quarter A vector of numerics give years and quarters.
#' @export
#' @examples
#'
#' make_yearquarter(year = 2021, quarter = 2:3)
#' make_yearquarter(year = 2020:2021, quarter = 2:3)
make_yearquarter <- function(year = 1970L, quarter = 1L, fiscal_start = 1) {
  lst <- vec_recycle_common(year = year, quarter = quarter)
  if (any(lst$quarter > 4 | lst$quarter < 1)) {
    abort("Quarters can't be less than 1 or greater than 4.")
  }
  yearquarter(4 * (lst$year - 1970) + lst$quarter - 1, fiscal_start)
}

#' @export
yearquarter.default <- function(x, fiscal_start = 1) {
  dont_know(x, "yearquarter")
}

#' @export
yearquarter.NULL <- function(x, fiscal_start = 1) {
  new_yearquarter(double(), fiscal_start = 1)
}

#' @export
yearquarter.logical <- function(x, ...) {
  if (is.logical(x) && all(is.na(x))) {
    new_yearquarter(0) + NA_real_
  } else {
    dont_know(x, "yearquarter")
  }
}

#' @export
yearquarter.POSIXct <- function(x, fiscal_start = 1) {
  yr <- year(x)
  mth <- fiscal_start + (month(x) - fiscal_start) %/% 3 * 3
  mth0 <- mth == 0
  mth1 <- mth == -1
  mth[mth0] <- 12
  mth[mth1] <- 11
  lgl <- mth0 | mth1
  vec_slice(yr, lgl) <- vec_slice(yr, lgl) - 1
  new_yearquarter(make_date(yr, mth), fiscal_start)
}

#' @export
yearquarter.POSIXlt <- yearquarter.POSIXct

#' @export
yearquarter.Date <- yearquarter.POSIXct

#' @export
yearquarter.character <- function(x, fiscal_start = 1) {
  # exact matching with q, qtr, or quarter
  key_words <- regmatches(x, gregexpr("[[:alpha:]]+", x))
  if (all(grepl("^(q|qtr|quarter)$", key_words, ignore.case = TRUE))) {
    yr_qtr <- regmatches(x, gregexpr("[[:digit:]]+", x))
    digits_lgl <- map_lgl(yr_qtr, function(.x) !has_length(.x, 2))
    digits_len <- map_int(yr_qtr, function(.x) sum(nchar(.x)))
    if (any(digits_lgl) || any(digits_len != 5)) {
      abort("Character strings are not in a standard unambiguous format.")
    }
    yr_lgl <- map(yr_qtr, function(.x) grepl("[[:digit:]]{4}", .x))
    yr <- as.integer(map2_chr(yr_qtr, yr_lgl, function(.x, .y) .x[.y]))
    qtr <- as.integer(map2_chr(yr_qtr, yr_lgl, function(.x, .y) .x[!.y]))
    if (any(qtr > 4 | qtr < 1)) {
      abort("Quarters can't be less than 1 or greater than 4.")
    }
    yearquarter(4 * (yr - 1970) + qtr - 1, fiscal_start)
  } else {
    assertDate(x)
    yearquarter(anydate(x), fiscal_start)
  }
}

#' @export
yearquarter.yearweek <- yearquarter.POSIXct

#' @export
yearquarter.yearmonth <- yearquarter.POSIXct

#' @export
yearquarter.yearquarter <- function(x, fiscal_start = attr(x, "fiscal_start")) {
  fs <- fiscal_start(x)
  mth <- fiscal_start - fs
  new_yearquarter(
    new_date(x) + period(year = -(fs == 1) + (fiscal_start == 1), month = mth),
    fiscal_start)
}

#' @export
yearquarter.numeric <- function(x, fiscal_start = 1) {
  date0 <- make_date(1969 + as.integer(fiscal_start == 1), fiscal_start)
  new_yearquarter(date0 + period(month = x * 3), fiscal_start)
}

#' @export
yearquarter.yearqtr <- function(x, fiscal_start = 1) {
  year <- trunc(x)
  last_month <- trunc((x %% 1) * 4 + 1) * 3
  first_month <- last_month - 2
  result <- make_date(year, first_month, 1)
  new_yearquarter(result, fiscal_start)
}

new_yearquarter <- function(x = double(), fiscal_start = 1) {
  if (!has_length(fiscal_start, 1)) {
    abort("`fiscal_start` must be of length 1.")
  }
  if (fiscal_start < 1 || fiscal_start > 12) {
    abort("`fiscal_start` only accepts a value between 1 and 12.")
  }
  new_vctr(x, fiscal_start = fiscal_start, class = "yearquarter")
}

fiscal_start <- function(x) {
  attr(x, "fiscal_start") %||% 1
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
#' @method vec_cast yearquarter
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
  base <- yearquarter(0, fiscal_start(x))
  4 * (year(x) - year(base)) + quarter(x) - quarter(base)
}

#' @export
vec_cast.POSIXlt.yearquarter <- function(x, to, ...) {
  as.POSIXlt(new_date(x), ...)
}

#' @export
vec_cast.yearquarter.yearquarter <- function(x, to, ...) {
  yearquarter(x, fiscal_start(to))
}

#' @export
vec_cast.character.yearquarter <- function(x, to, ...) {
  format(x)
}

#' @rdname tsibble-vctrs
#' @method vec_ptype2 yearquarter
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
  if (fiscal_start(x) != fiscal_start(y)) {
    abort("Can't combine <yearquarter> with different `fiscal_start`.")
  }
  new_yearquarter(fiscal_start = fiscal_start(x))
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
    new_yearquarter(as_date(x) + period(months = y * 3), fiscal_start(x))
  } else if (op == "-") {
    new_yearquarter(as_date(x) - period(months = y * 3), fiscal_start(x))
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
    yearquarter(period(months = x * 3) + as_date(y), fiscal_start(y))
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
  fs <- fiscal_start(x)
  yrqtr <- quarter(x, with_year = TRUE, fiscal_start = fs)
  yr <- trunc(yrqtr)
  qtr <- round(yrqtr %% 1 * 10)
  qtr_sub <- map_chr(formatC(qtr), function(z) gsub("%q", z, x = format))
  qtr_sub[is.na(qtr_sub)] <- "-" # NA formats cause errors
  format.Date(make_date(yr, qtr * 3 - 2), format = qtr_sub)
}

#' @rdname tsibble-vctrs
#' @export
obj_print_data.yearquarter <- function(x, ...) {
  if (length(x) == 0) return()
  print(format(x))
}

#' @export
obj_print_footer.yearquarter <- function(x, ...) {
  cat_line("# Year starts on: ", fmt_month(fiscal_start(x)))
}

fmt_month <- function(x) {
  month.name[x]
}

#' @export
vec_ptype_abbr.yearquarter <- function(x, ...) {
  "qtr"
}

#' @export
seq.yearquarter <- function(from, to, by, length.out = NULL, along.with = NULL,
                            ...) {
  fs <- fiscal_start(from)
  from <- vec_cast(from, new_date())
  if (!is_missing(to)) {
    to <- vec_cast(to, new_date())
  }
  if (is_missing(by)) {
    new_yearquarter(seq_date(
      from = from, to = to, length.out = length.out,
      along.with = along.with, ...
    ), fs)
  } else {
    bad_by(by)
    by_qtr <- paste(by, "quarter")
    new_yearquarter(seq_date(
      from = from, to = to, by = by_qtr, length.out = length.out,
      along.with = along.with, ...
    ), fs)
  }
}

seq.ordered <- function(from, to, by, ...) {
  bad_by(by)
  lvls <- levels(from)
  idx_from <- which(lvls %in% from)
  idx_to <- which(lvls %in% to)
  idx <- seq.int(idx_from, idx_to, by = by)
  ordered(lvls[idx], levels = lvls)
}

#' @rdname year-quarter
#' @export
#' @examples
#'
#' # `fiscal_year()` helps to extract fiscal year
#' y <- yearquarter(as.Date("2020-06-01"), fiscal_start = 6)
#' fiscal_year(y)
#' lubridate::year(y) # calendar years
fiscal_year <- function(x) {
  stopifnot(is_yearquarter(x))
  trunc(quarter(x, TRUE, fiscal_start(x)))
}

#' @export
union.yearquarter <- set_ops("yearquarter", op = "union")

#' @export
intersect.yearquarter <- set_ops("yearquarter", op = "intersect")

#' @export
setdiff.yearquarter <- set_ops("yearquarter", op = "setdiff")

