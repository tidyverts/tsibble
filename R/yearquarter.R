#' Represent year-quarter
#'
#' @description
#' \lifecycle{stable}
#'
#' Create or coerce using `yearquarter()`.
#'
#' @param x Other object.
#'
#' @return year-quarter (`yearquarter`) objects.
#'
#' @family index functions
#' @seealso [interval_pull], [units_since]
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
yearquarter.POSIXt <- function(x) {
  new_yearquarter(as_date(floor_date(x, unit = "quarters")))
}

#' @export
yearquarter.Date <- yearquarter.POSIXt

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
yearquarter.yearweek <- yearquarter.POSIXt

#' @export
yearquarter.yearmonth <- yearquarter.POSIXt

#' @export
yearquarter.yearquarter <- function(x) {
  new_yearquarter(x)
}

#' @export
yearquarter.numeric <- function(x) {
  year <- trunc(x)
  last_month <- trunc((x %% 1) * 4 + 1) * 3
  first_month <- formatC(last_month - 2, flag = 0, width = 2)
  result <- make_date(year, first_month, 1)
  new_yearquarter(result)
}

#' @export
yearquarter.yearqtr <- yearquarter.numeric

new_yearquarter <- function(x = double()) {
  new_vctr(x, class = "yearquarter")
}

#' @rdname year-quarter
#' @export
is_yearquarter <- function(x) {
  inherits(x, "yearquarter")
}

diff.yearquarter <- function(x, lag = 1, differences = 1, ...) {
  out <- diff((year(x) - 1970) * 4 + quarter(x),
    lag = lag, differences = differences
  )
  structure(out, class = "difftime", units = "quarters")
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_cast yearquarter
#' @export
#' @export vec_cast.yearquarter
vec_cast.yearquarter <- function(x, to, ...) {
  UseMethod("vec_cast.yearquarter")
}

#' @export
as.Date.yearquarter <- function(x, ...) {
  new_date(x)
}

#' @method vec_cast.Date yearquarter
#' @export
vec_cast.Date.yearquarter <- function(x, to, ...) {
  new_date(x)
}

#' @method vec_cast.POSIXct yearquarter
#' @export
vec_cast.POSIXct.yearquarter <- function(x, to, ...) {
  as.POSIXct(new_date(x), ...)
}

#' @method vec_cast.double yearquarter
#' @export
vec_cast.double.yearquarter <- function(x, to, ...) {
  as.double((year(x) - 1970) * 4 + quarter(x) - 1)
}

#' @export
as.POSIXlt.yearquarter <- function(x, tz = "", ...) {
  as.POSIXlt(new_date(x), tz = tz, ...)
}

#' @method vec_cast.POSIXlt yearquarter
#' @export
vec_cast.POSIXlt.yearquarter <- function(x, to, ...) { # not working
  as.POSIXlt(new_date(x), ...)
}

#' @method vec_cast.yearquarter yearquarter
#' @export
vec_cast.yearquarter.yearquarter <- function(x, to, ...) {
  new_yearquarter(x)
}

#' @method vec_cast.character yearquarter
#' @export
vec_cast.character.yearquarter <- function(x, to, ...) {
  format.Date(x)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype2 yearquarter
#' @export
#' @export vec_ptype2.yearquarter
vec_ptype2.yearquarter <- function(x, y, ...) {
  UseMethod("vec_ptype2.yearquarter", y)
}

#' @method vec_ptype2.yearquarter POSIXt
#' @export
vec_ptype2.yearquarter.POSIXt <- function(x, y, ...) {
  new_datetime()
}

#' @method vec_ptype2.POSIXt yearquarter
#' @export
vec_ptype2.POSIXt.yearquarter <- function(x, y, ...) {
  new_datetime()
}

#' @method vec_ptype2.yearquarter Date
#' @export
vec_ptype2.yearquarter.Date <- function(x, y, ...) {
  new_date()
}

#' @method vec_ptype2.yearquarter yearquarter
#' @export
vec_ptype2.yearquarter.yearquarter <- function(x, y, ...) {
  new_yearquarter()
}

#' @method vec_ptype2.Date yearquarter
#' @export
vec_ptype2.Date.yearquarter <- function(x, y, ...) {
  new_date()
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_arith yearquarter
#' @export
#' @export vec_arith.yearquarter
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
  year <- year(x)
  year_sym <- "%Y"
  if (grepl("%y", format)) {
    year <- sprintf("%02d", year %% 100)
    year_sym <- "%y"
  } else if (grepl("%C", format)) {
    year <- year %/% 100
    year_sym <- "%C"
  }
  qtr <- quarter(x)
  qtr_sub <- map_chr(qtr, ~ gsub("%q", ., x = format))
  year_sub <- map2_chr(year, qtr_sub, ~ gsub(year_sym, .x, x = .y))
  year_sub
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method obj_print_data yearquarter
#' @export
#' @export obj_print_data.yearquarter
obj_print_data.yearquarter <- function(x, ...) {
  if (length(x) == 0) return()
  print(format(x))
}

#' @export
vec_ptype_abbr.yearquarter <- function(x, ...) {
  "mth"
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
