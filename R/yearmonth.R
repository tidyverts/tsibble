yearmonth <- function(x = double()) {
  if (is_yearmonth(x)) return(x)
  if (is.POSIXt(x)) {
    out <- as_date(floor_date(x, unit = "months"))
  } else if (is.Date(x)) {
    out <- floor_date(x, unit = "months")
  } else if (is.numeric(x)) {
    stopifnot(x > 1581 && x < 2500)
    year <- trunc(x)
    month <- formatC(round((x %% 1) * 12) %% 12 + 1, flag = 0, width = 2)
    out <- make_date(year, month, 1)
  } else if (is.character(x)) {
    assertDate(x)
    out <- anydate(x)
  } else {
    dont_know(x, "yearmonth")
  }
  new_yearmonth(out)
}

new_yearmonth <- function(x = double()) {
  # vec_assert(x, double())
  new_vctr(x, class = "yearmonth")
}

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
  as.POSIXct(new_date(x))
}

#' @export
as.POSIXlt.yearmonth <- function(x, tz = "", ...) {
  as.POSIXlt(new_date(x), tz = tz, ...)
}

#' @method vec_cast.POSIXlt yearmonth
#' @export
vec_cast.POSIXlt.yearmonth <- function(x, to, ...) { # not working
  as.POSIXlt(new_date(x))
}

#' @method vec_cast.double yearmonth
#' @export
vec_cast.double.yearmonth <- function(x, to, ...) {
  as.double((year(x) - 1970) * 12 + month(x) - 1)
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
    yearmonth(period(months = e1) + as_date(e2))
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

#' @export
vec_ptype_abbr.yearmonth <- function(x, ...) {
  "mth"
}

#' @export
seq.yearmonth <- function(from, to, by, length.out = NULL, along.with = NULL,
                          ...) {
  bad_by(by)
  by_mth <- paste(by, "month")
  new_yearmonth(seq_date(
    from = from, to = to, by = by_mth, length.out = length.out,
    along.with = along.with, ...
  ))
}
