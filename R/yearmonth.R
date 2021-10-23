#' Represent year-month
#'
#' @description
#' \lifecycle{stable}
#'
#' Create or coerce using `yearmonth()`.
#'
#' @section Display:
#' Use `format()` to display `yearweek`, `yearmonth`, and `yearquarter` objects
#' in required formats.
#' Please see [`strptime()`] details for supported conversion specifications.
#'
#' @param x Other object.
#' @param format A vector of strings to specify additional formats of `x` (e.g. `%Y%m`),
#' if a warning or an error occurs.
#' @param ... Further arguments to methods.
#'
#' @return year-month (`yearmonth`) objects.
#'
#' @seealso [scale_x_yearmonth] and others for ggplot2 scales
#' @family index functions
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
#' # seq() and arithmetic
#' mth <- yearmonth("2017-11")
#' seq(mth, length.out = 10, by = 1) # by 1 month
#' mth + 0:9
#'
#' # display formats
#' format(mth, format = "%y %m")
#'
#' # units since 1970 Jan
#' as.double(yearmonth("1969 Jan") + 0:24)
yearmonth <- function(x, ...) {
  UseMethod("yearmonth")
}

#' @rdname year-month
#' @param year,month A vector of numerics give years and months.
#' @export
#' @examples
#'
#' make_yearmonth(year = 2021, month = 10:11)
#' make_yearmonth(year = 2020:2021, month = 10:11)
make_yearmonth <- function(year = 1970L, month = 1L) {
  lst <- vec_recycle_common(year = year, month = month)
  new_yearmonth(make_date(lst$year, lst$month, 1))
}

#' @export
yearmonth.default <- function(x, ...) {
  dont_know(x, "yearmonth")
}

#' @export
yearmonth.NULL <- function(x, ...) {
  new_yearmonth()
}

#' @export
yearmonth.logical <- function(x, ...) {
  if (is.logical(x) && all(is.na(x))) {
    new_yearmonth(0) + NA_real_
  } else {
    dont_know(x, "yearmonth")
  }
}

#' @export
yearmonth.POSIXct <- function(x, ...) {
  new_yearmonth(floor_date(as_date(x), unit = "months"))
}

#' @export
yearmonth.POSIXlt <- yearmonth.POSIXct

#' @export
yearmonth.Date <- function(x, ...) {
  new_yearmonth(floor_date(x, unit = "months"))
}

#' @rdname year-month
#' @export
yearmonth.character <- function(x, format = NULL, ...) {
  fmts <- c("%B %Y", "%b %Y", "%Y M%m", "%Y m%m")
  dates <- with_anytime_formats({
    assertDate(x)
    anydate(x)
  }, formats_before = format, formats_after = fmts)
  if (is_null(format)) {
    if (any(!grepl("\\D", x))) { # all numbers without delimiter
      warn(c(
        "`yearmonth()` may yield unexpected results.", 
        i = "Please use arg `format` to supply formats."))
    }
  }
  yearmonth(dates)
}

#' @export
yearmonth.yearweek <- yearmonth.POSIXct

#' @export
yearmonth.yearmonth <- function(x, ...) {
  x
}

#' @export
yearmonth.numeric <- function(x, ...) {
  new_yearmonth(0) + x
}

#' @export
yearmonth.yearmon <- function(x, ...) {
  year <- trunc(x)
  month <- formatC(round((x %% 1) * 12) %% 12 + 1, flag = 0, width = 2)
  result <- make_date(year, month, 1)
  new_yearmonth(result)
}

new_yearmonth <- function(x = double()) {
  new_vctr(x, class = "yearmonth")
}

#' @rdname year-month
#' @export
is_yearmonth <- function(x) {
  inherits(x, "yearmonth")
}

#' @export
is.numeric.yearmonth <- function(x) {
  FALSE
}

#' @export
tz.yearmonth <- function(x) {
  "UTC"
}

# diff.yearmonth <- function(x, lag = 1, differences = 1, ...) {
#   out <- diff((year(x) - 1970) * 12 + month(x),
#     lag = lag, differences = differences
#   )
#   structure(out, class = "difftime", units = "months")
# }

#' @rdname tsibble-vctrs
#' @export
vec_cast.yearmonth <- function(x, to, ...) {
  UseMethod("vec_cast.yearmonth")
}

#' @export
vec_cast.Date.yearmonth <- function(x, to, ...) {
  new_date(x)
}

#' @export
vec_cast.POSIXct.yearmonth <- function(x, to, ...) {
  as.POSIXct(new_date(x), ...)
}

#' @export
vec_cast.double.yearmonth <- function(x, to, ...) {
  as.double((year(x) - 1970) * 12 + month(x) - 1)
}

#' @export
vec_cast.POSIXlt.yearmonth <- function(x, to, ...) {
  as.POSIXlt(new_date(x), ...)
}

#' @export
vec_cast.yearmonth.yearmonth <- function(x, to, ...) {
  new_yearmonth(x)
}

#' @export
vec_cast.character.yearmonth <- function(x, to, ...) {
  format(x)
}

#' @rdname tsibble-vctrs
#' @export
vec_ptype2.yearmonth <- function(x, y, ...) {
  UseMethod("vec_ptype2.yearmonth", y)
}

#' @export
vec_ptype2.yearmonth.POSIXct <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.POSIXct.yearmonth <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.yearmonth.Date <- function(x, y, ...) {
  new_date()
}

#' @export
vec_ptype2.yearmonth.yearmonth <- function(x, y, ...) {
  new_yearmonth()
}

#' @export
vec_ptype2.Date.yearmonth <- function(x, y, ...) {
  new_date()
}

#' @rdname tsibble-vctrs
#' @method vec_arith yearmonth
#' @export
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
    new_yearmonth(as_date(x) - period(months = y))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yearmonth yearmonth
#' @export
vec_arith.yearmonth.yearmonth <- function(op, x, y, ...) {
  if (op == "-") {
    as.double(x) - as.double(y)
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

#' @rdname tsibble-vctrs
#' @export
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
  from <- vec_cast(from, new_date())
  if (!is_missing(to)) {
    to <- vec_cast(to, new_date())
  }
  if (is_missing(by)) {
    new_yearmonth(seq_date(
      from = from, to = to, length.out = length.out,
      along.with = along.with, ...
    ))
  } else {
    bad_by(by)
    by_mth <- paste(by, "month")
    new_yearmonth(seq_date(
      from = from, to = to, by = by_mth, length.out = length.out,
      along.with = along.with, ...
    ))
  }
}


#' @importFrom generics union
#' @export
generics::union

#' @importFrom generics intersect
#' @export
generics::intersect

#' @importFrom generics setdiff
#' @export
generics::setdiff

set_ops <- function(class = "yearmonth", op = "intersect") {
  force(class)
  force(op)
  fun <- switch(op,
    "union" = function(x, y, ...) vec_unique(vec_c(x, y)),
    "intersect" = function(x, y, ...) vec_slice(x, vec_in(x, y)),
    "setdiff" = function(x, y, ...)
      vec_unique(if (length(x) || length(y)) x[is.na(vec_match(x, y))] else x)
  )
  function(x, y, ...) {
    abort_if_not(y, class)
    fun(x, y, ...)
  }
}

#' @export
union.yearmonth <- set_ops("yearmonth", op = "union")

#' @export
intersect.yearmonth <- set_ops("yearmonth", op = "intersect")

#' @export
setdiff.yearmonth <- set_ops("yearmonth", op = "setdiff")

bad_by <- function(by) {
  if (!is_bare_numeric(by, n = 1)) {
    abort("`by` only takes a numeric.")
  }
}

# nocov start
seq_date <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
  if (missing(from)) {
    stop("'from' must be specified")
  }
  if (!inherits(from, "Date")) {
    stop("'from' must be a \"Date\" object")
  }
  if (length(as.Date(from)) != 1L) {
    stop("'from' must be of length 1")
  }
  if (!missing(to)) {
    if (!inherits(to, "Date")) {
      stop("'to' must be a \"Date\" object")
    }
    if (length(as.Date(to)) != 1L) {
      stop("'to' must be of length 1")
    }
  }
  if (!is.null(along.with)) { # !missing(along.with) in seq.Date
    length.out <- length(along.with)
  } else if (!is.null(length.out)) {
    if (length(length.out) != 1L) {
      stop("'length.out' must be of length 1")
    }
    length.out <- ceiling(length.out)
  }
  status <- c(!missing(to), !missing(by), !is.null(length.out))
  if (sum(status) != 2L) {
    stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
  }
  if (missing(by)) {
    from <- unclass(as.Date(from))
    to <- unclass(as.Date(to))
    res <- as.double(seq.int(from, to, length.out = length.out))
    return(new_date(res))
  }
  if (length(by) != 1L) {
    stop("'by' must be of length 1")
  }
  valid <- 0L
  if (inherits(by, "difftime")) {
    by <- switch(attr(by, "units"), secs = 1 / 86400, mins = 1 / 1440,
      hours = 1 / 24, days = 1, weeks = 7
    ) * unclass(by)
  } else if (is.character(by)) {
    by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L) {
      stop("invalid 'by' string")
    }
    valid <- pmatch(by2[length(by2)], c(
      "days", "weeks",
      "months", "quarters", "years"
    ))
    if (is.na(valid)) {
      stop("invalid string for 'by'")
    }
    if (valid <= 2L) {
      by <- c(1, 7)[valid]
      if (length(by2) == 2L) {
        by <- by * as.integer(by2[1L])
      }
    } else {
      by <- if (length(by2) == 2L) {
        as.integer(by2[1L])
      } else {
        1
      }
    }
  } else if (!is.numeric(by)) {
    stop("invalid mode for 'by'")
  }
  if (is.na(by)) {
    stop("'by' is NA")
  }
  if (valid <= 2L) {
    from <- unclass(as.Date(from))
    if (!is.null(length.out)) {
      res <- seq.int(from, by = by, length.out = length.out)
    } else {
      to0 <- unclass(as.Date(to))
      res <- seq.int(0, to0 - from, by) + from
    }
  } else {
    r1 <- as.POSIXlt(from)
    if (valid == 5L) {
      if (missing(to)) {
        yr <- seq.int(r1$year, by = by, length.out = length.out)
      } else {
        to0 <- as.POSIXlt(to)
        yr <- seq.int(r1$year, to0$year, by)
      }
      r1$year <- yr
      res <- as.Date(r1)
    } else {
      if (valid == 4L) {
        by <- by * 3
      }
      if (missing(to)) {
        mon <- seq.int(r1$mon, by = by, length.out = length.out)
      }
      else {
        to0 <- as.POSIXlt(to)
        mon <- seq.int(r1$mon, 12 * (to0$year - r1$year) +
          to0$mon, by)
      }
      r1$mon <- mon
      res <- as.Date(r1)
    }
  }
  if (!missing(to)) {
    to <- as.Date(to)
    res <- if (by > 0) {
      res[res <= to]
    } else {
      res[res >= to]
    }
  }
  new_date(as.double(res))
}
# nocov end
