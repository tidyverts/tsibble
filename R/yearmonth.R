#' Represent year-month
#'
#' @description
#' \lifecycle{stable}
#'
#' Create or coerce using `yearmonth()`.
#'
#' @param x Other object.
#'
#' @return year-month (`yearmonth`) objects.
#'
#' @seealso [interval_pull], [units_since]
#' @rdname year-month
#' @export
#' @examples
#' # coerce POSIXct/Dates to yearmonth
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 month")
#' yearmonth(x)
#'
#' # parse characters
#' yearmonth(c("2018 Jan", "2018-01", "2018 January"))
#'
#' # creat an empty yearmonth container
#' yearmonth()
#'
#' # seq() and arithmetic
#' mth <- yearmonth("2017-11")
#' seq(mth, length.out = 10, by = 1) # by 1 month
#' mth + 0:9
#'
#' # display formats
#' format(mth, format = "%y %m")
yearmonth <- function(x) {
  UseMethod("yearmonth")
}

#' @export
yearmonth.default <- function(x) {
  dont_know(x, "yearmonth")
}

#' @export
yearmonth.NULL <- function(x) {
  new_yearmonth()
}

#' @export
yearmonth.POSIXt <- function(x) {
  new_yearmonth(as_date(floor_date(x, unit = "months")))
}

#' @export
yearmonth.Date <- function(x) {
  new_yearmonth(floor_date(x, unit = "months"))
}

#' @export
yearmonth.character <- function(x) {
  assertDate(x)
  new_yearmonth(anydate(x))
}

#' @export
yearmonth.yearweek <- yearmonth.Date

#' @export
yearmonth.yearmonth <- function(x) {
  new_yearmonth(x)
}

#' @export
yearmonth.numeric <- function(x) {
  year <- trunc(x)
  month <- formatC(round((x %% 1) * 12) %% 12 + 1, flag = 0, width = 2)
  result <- make_date(year, month, 1)
  new_yearmonth(result)
}

#' @export
yearmonth.yearmon <- yearmonth.numeric

new_yearmonth <- function(x = double()) {
  # vec_assert(x, double())
  new_vctr(x, class = "yearmonth")
}

#' @rdname year-month
#' @export
is_yearmonth <- function(x) {
  inherits(x, "yearmonth")
}

diff.yearmonth <- function(x, lag = 1, differences = 1, ...) {
  out <- diff((year(x) - 1970) * 12 + month(x),
    lag = lag, differences = differences
  )
  structure(out, class = "difftime", units = "months")
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_cast yearmonth
#' @export
#' @export vec_cast.yearmonth
vec_cast.yearmonth <- function(x, to, ...) {
  UseMethod("vec_cast.yearmonth")
}

#' @method vec_cast.Date yearmonth
#' @export
vec_cast.Date.yearmonth <- function(x, to, ...) {
  new_date(x)
}

#' @method vec_cast.POSIXct yearmonth
#' @export
vec_cast.POSIXct.yearmonth <- function(x, to, ...) {
  as.POSIXct(new_date(x), ...)
}

#' @method vec_cast.character yearmonth
#' @export
vec_cast.character.yearmonth <- function(x, to, ...) {
  format(x)
}

#' @method vec_cast.double yearmonth
#' @export
vec_cast.double.yearmonth <- function(x, to, ...) {
  vec_data(x)
}

#' @export
as.POSIXlt.yearmonth <- function(x, tz = "", ...) {
  as.POSIXlt(new_date(x), tz = tz, ...)
}

#' @method vec_cast.POSIXlt yearmonth
#' @export
vec_cast.POSIXlt.yearmonth <- function(x, to, ...) { # not working
  as.POSIXlt(new_date(x), ...)
}

#' @method vec_cast.yearmonth yearmonth
#' @export
vec_cast.yearmonth.yearmonth <- function(x, y, ...) {
  new_yearmonth(x)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype2 yearmonth
#' @export
#' @export vec_ptype2.yearmonth
vec_ptype2.yearmonth <- function(x, y, ...) {
  UseMethod("vec_ptype2.yearmonth", y)
}

#' @method vec_ptype2.yearmonth POSIXt
#' @export
vec_ptype2.yearmonth.POSIXt <- function(x, y, ...) {
  new_datetime()
}

#' @method vec_ptype2.POSIXt yearmonth
#' @export
vec_ptype2.POSIXt.yearmonth <- function(x, y, ...) {
  new_datetime()
}

#' @method vec_ptype2.yearmonth Date
#' @export
vec_ptype2.yearmonth.Date <- function(x, y, ...) {
  new_date()
}

#' @method vec_ptype2.yearmonth yearmonth
#' @export
vec_ptype2.yearmonth.yearmonth <- function(x, y, ...) {
  new_yearmonth()
}

#' @method vec_ptype2.Date yearmonth
#' @export
vec_ptype2.Date.yearmonth <- function(x, y, ...) {
  new_date()
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_arith yearmonth
#' @export
#' @export vec_arith.yearmonth
vec_arith.yearmonth <- function(op, x, y, ...) {
  UseMethod("vec_arith.yearmonth", y)
}

#' @method vec_arith.yearmonth default
#' @export
vec_arith.yearmonth.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.yearmonth numeric
#' @export
vec_arith.yearmonth.numeric <- function(op, x, y, ...) {
  if (op == "+") {
    new_yearmonth(as_date(x) + period(months = y))
  } else if (op == "-") {
    new_yearmonth(as_date(x) - period(months = y, units = "month"))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.numeric yearmonth
#' @export
vec_arith.numeric.yearmonth <- function(op, x, y, ...) {
  if (op == "+") {
    yearmonth(period(months = x) + as_date(y))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yearmonth MISSING
#' @export
vec_arith.yearmonth.MISSING <- function(op, x, y, ...) {
  switch(op, 
    `-` = x,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @export
format.yearmonth <- function(x, format = "%Y %b", ...) {
  format.Date(new_date(x), format = format, ...)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method obj_print_data yearmonth
#' @export
#' @export obj_print_data.yearmonth
obj_print_data.yearmonth <- function(x, ...) {
  if (length(x) == 0) return()
  print(format(x))
}

#' @export
vec_ptype_abbr.yearmonth <- function(x, ...) {
  "mth"
}

#' @export
seq.yearmonth <- function(from, to, by, length.out = NULL, along.with = NULL,
                          ...) {
  bad_by(by)
  by_mth <- paste(by, "month")
  from <- vec_cast(from, new_date())
  if (!is_missing(to)) {
    to <- vec_cast(to, new_date())
  }
  new_yearmonth(seq_date(
    from = from, to = to, by = by_mth, length.out = length.out,
    along.with = along.with, ...
  ))
}
