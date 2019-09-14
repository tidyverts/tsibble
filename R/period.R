# Unlike zoo::yearmon and zoo::yearqtr based on numerics,
# tsibble::yearmonth and tsibble::yearquarter are based on the "Date" class.

#' Represent year-week (ISO) starting on Monday, year-month or year-quarter objects
#'
#' @description
#' \lifecycle{stable}
#'
#' Create or coerce using `yearweek()`, `yearmonth()`, or `yearquarter()`
#'
#' @param x Other object.
#'
#' @return Year-week (`yearweek`), year-month (`yearmonth`) or year-quarter
#' (`yearquarter`) objects.
#'
#' @section Index functions:
#' The tsibble `yearmonth()` and `yearquarter()` function respects time zones of
#' the input `x`, contrasting to their zoo counterparts.
#'
#' @export
#' @seealso [interval_pull]
#'
#' @examples
#' # coerce POSIXct/Dates to yearweek, yearmonth, yearquarter
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 month")
#' yearweek(x)
#' yearmonth(x)
#' yearmonth(yearweek(x))
#' yearquarter(x)
#'
#' # coerce yearmonths to yearquarter
#' y <- yearmonth(x)
#' yearquarter(y)
#'
#' # parse characters
#' yearweek(c("2018 W01", "2018 Wk01", "2018 Week 1"))
#' yearmonth(c("2018 Jan", "2018-01", "2018 January"))
#' yearquarter(c("2018 Q1", "2018 Qtr1", "2018 Quarter 1"))
#'
#' # seq() and binary operaters
#' wk1 <- yearweek("2017-11-01")
#' wk2 <- yearweek("2018-04-29")
#' seq(from = wk1, to = wk2, by = 2) # by two weeks
#' wk1 + 0:9
#' mth <- yearmonth("2017-11")
#' seq(mth, length.out = 5, by = 1) # by 1 month
#' mth + 0:9
#' seq(yearquarter(mth), length.out = 5, by = 1) # by 1 quarter
#'
#' # different formats
#' format(c(wk1, wk2), format = "%V/%Y")
#' format(y, format = "%y %m")
#' format(yearquarter(mth), format = "%y Qtr%q")
#' @rdname period
#' @export
yearquarter <- function(x) {
  UseMethod("yearquarter")
}

new_yearquarter <- function(x) {
  structure(x, tz = NULL, class = c("yearquarter", "Date"))
}

#' @export
c.yearquarter <- function(..., recursive = FALSE) {
  new_yearquarter(NextMethod())
}

#' @export
rep.yearquarter <- function(x, ...) {
  new_yearquarter(NextMethod())
}

#' @export
unique.yearquarter <- function(x, incomparables = FALSE, ...) {
  new_yearquarter(NextMethod())
}

#' @export
diff.yearquarter <- function(x, lag = 1, differences = 1, ...) {
  out <- diff((year(x) - 1970) * 4 + quarter(x),
    lag = lag, differences = differences
  )
  structure(out, class = "difftime", units = "quarters")
}

#' @export
`+.yearquarter` <- function(e1, e2) {
  if (nargs() == 1L) {
    return(e1)
  }
  e1_yrqtr <- is_yearquarter(e1)
  e2_yrqtr <- is_yearquarter(e2)
  if (e1_yrqtr && e2_yrqtr) {
    abort("Binary `+` is not defined for class yearquarter.")
  }
  if (e1_yrqtr) {
    new_yearquarter(as_date(e1) + period(months = e2 * 3))
  } else {
    yearquarter(period(months = e1 * 3) + as_date(e2))
  }
}

#' @export
`-.yearquarter` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)

  e1_yrqtr <- is_yearquarter(e1)
  e2_yrqtr <- is_yearquarter(e2)
  if (e1_yrqtr && e2_yrqtr) {
    res <- units_since(e1) - units_since(e2)
    structure(res, class = "difftime", units = "quarters")
  } else {
    new_yearquarter(as_date(e1) - period(months = e2 * 3))
  }
}

is_yearquarter <- function(x) {
  inherits(x, "yearquarter")
}

#' @export
yearquarter.default <- function(x) {
  dont_know(x, "yearquarter")
}

#' @export
yearquarter.POSIXt <- function(x) {
  new_yearquarter(as_date(floor_date(x, unit = "quarters")))
}

#' @export
yearquarter.Date <- yearquarter.POSIXt

#' @export
yearquarter.character <- function(x) {
  if (is_empty(x)) return(new_yearquarter(x))

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

as_date.yearquarter <- function(x, ...) {
  class(x) <- "Date"
  x
}

as_date.yearmonth <- as_date.yearquarter

as_date.yearweek <- as_date.yearquarter

#' @export
as.Date.yearquarter <- as_date.yearquarter

#' @export
as.Date.yearmonth <- as_date.yearmonth

#' @export
as.Date.yearweek <- as_date.yearweek

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

#' @export
seq.yearquarter <- function(from, to, by, length.out = NULL, along.with = NULL,
                            ...) {
  bad_by(by)
  by_qtr <- paste(by, "quarter")
  new_yearquarter(seq_date(
    from = from, to = to, by = by_qtr, length.out = length.out,
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

#' @export
`[.yearweek` <- function(x, ..., drop = TRUE) {
  new_yearweek(NextMethod())
}

#' @export
`[.yearquarter` <- function(x, ..., drop = TRUE) {
  new_yearquarter(NextMethod())
}

#' @export
as.POSIXlt.yearquarter <- function(x, tz = "", ...) {
  as.POSIXlt(as_date(x), tz = tz, ...)
}

#' Time units since Unix Epoch
#'
#' \lifecycle{questioning}
#'
#' @param x An object of `POSIXct`, `Date`, `yearweek`, `yearmonth`, `yearquarter`.
#'
#' @details
#' origin:
#' * `POSIXct`: 1970-01-01 00:00:00
#' * `Date`: 1970-01-01
#' * `yearweek`: 1970 W01 (i.e. 1969-12-29)
#' * `yearmonth`: 1970 Jan
#' * `yearquarter`: 1970 Qtr1
#' @export
#' @examples
#' units_since(x = yearmonth(2012 + (0:11) / 12))
units_since <- function(x) {
  UseMethod("units_since")
}

#' @export
units_since.numeric <- function(x) {
  if (min0(x) > 1581 && max0(x) < 2500) { # Input is years
    x - 1970L
  } else {
    x
  }
}

#' @export
units_since.yearweek <- function(x) {
  as.numeric((as_date(x) - as_date("1969-12-29")) / 7)
}

#' @export
units_since.yearmonth <- function(x) {
  as.numeric((year(x) - 1970) * 12 + month(x) - 1)
}

#' @export
units_since.yearquarter <- function(x) {
  as.numeric((year(x) - 1970) * 4 + quarter(x) - 1)
}

#' @export
units_since.Date <- function(x) {
  as.numeric(x)
}

#' @export
units_since.POSIXct <- function(x) {
  as.numeric(x)
}

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
    res <- seq.int(from, to, length.out = length.out)
    return(structure(res, class = "Date"))
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
    res <- structure(res, class = "Date")
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
  res
}
# nocov end
