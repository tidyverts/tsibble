# Unlike zoo::yearmon and zoo::yearqtr based on numerics,
# tsibble::yearmonth and tsibble::yearquarter are based on the "Date" class.

#' Represent year-week (ISO) starting on Monday, year-month or year-quarter objects
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
#' @rdname period
#' @seealso [interval_pull]
#'
#' @examples
#' # coerce POSIXct/Dates to yearweek, yearmonth, yearquarter
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 month")
#' yearweek(x)
#' yearmonth(x)
#' yearmonth(yearweek(x))
#' yearmonth("2018-07")
#' yearquarter(x)
#'
#' # coerce yearmonths to yearquarter
#' y <- yearmonth(x)
#' yearquarter(y)
#'
#' # seq() and binary operaters
#' wk1 <- yearweek("2017-11-01")
#' wk2 <- yearweek("2018-04-29")
#' seq(from = wk1, to = wk2, by = 2) # by two weeks
#' wk1 + 0:9
#' mth <- yearmonth("2017-11-01")
#' seq(mth, length.out = 5, by = 1) # by 1 month
#' mth + 0:9
#' seq(yearquarter(mth), length.out = 5, by = 1) # by 1 quarter
#'
#' # different formats
#' format(c(wk1, wk2), format = "%V/%Y")
#' format(y, format = "%y %m")
#' format(yearquarter(mth), format = "%y Qtr%q")
yearweek <- function(x) {
  UseMethod("yearweek")
}

as_yearweek <- function(x) {
  structure(x, tzone = NULL, class = c("yearweek", "Date"))
}

#' @export
c.yearweek <- function(..., recursive = FALSE) {
  as_yearweek(NextMethod())
}

#' @export
rep.yearweek <- function(x, ...) {
  as_yearweek(NextMethod())
}

#' @export
unique.yearweek <- function(x, incomparables = FALSE, ...) {
  as_yearweek(NextMethod())
}

#' @export
diff.yearweek <- function(x, lag = 1, differences = 1, ...) {
  out <- diff((as_date(x) - as_date("1969-12-29")) / 7,
    lag = lag, differences = differences)
  structure(out, class = "difftime", units = "weeks")
}

#' @export
`+.yearweek` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  e1_yrwk <- is_yearweek(e1)
  e2_yrwk <- is_yearweek(e2)
  if (e1_yrwk && e2_yrwk) {
    abort("Binary `+` is not defined for class yearweek.")
  }
  if (e1_yrwk) {
    yearweek(as_date(e1) + e2 * 7)
  } else {
    yearweek(e1 * 7 + as_date(e2))
  }
}

#' @export
`-.yearweek` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  e1_yrwk <- is_yearweek(e1)
  e2_yrwk <- is_yearweek(e2)
  if (e1_yrwk && e2_yrwk) {
    res <- units_since(e1) - units_since(e2)
    structure(res, class = "difftime", units = "weeks")
  } else {
    yearweek(as_date(e1) - e2 * 7)
  }
}

is_yearweek <- function(x) {
  inherits(x, "yearweek")
}

#' @export
yearweek.default <- function(x) {
  dont_know(x, "yearweek")
}

#' @export
yearweek.POSIXt <- function(x) {
  as_yearweek(as_date(lubridate::floor_date(x, unit = "weeks", week_start = 1)))
}

#' @export
yearweek.Date <- yearweek.POSIXt

#' @export
yearweek.character <- function(x) {
  if (is_empty(x)) return(as_yearweek(x))

  anytime::assertDate(x)
  yearweek(anytime::anydate(x))
}

#' @export
yearweek.yearweek <- function(x) {
  as_yearweek(x)
}

#' @export
format.yearweek <- function(x, format = "%Y W%V", ...) {
  x <- as_date(x)
  yr <- lubridate::year(x)
  ord <- lubridate::make_date(yr, 1)
  wday <- lubridate::wday(x, week_start = 1)
  mth_wk <- strftime(x, format = "%m_%V")
  yrs <- yr
  yrs[mth_wk == "01_53"] <- yr[mth_wk == "01_53"] - 1
  yrs[mth_wk == "12_01"] <- yr[mth_wk == "12_01"] + 1
  if (format == "%Y W%V") {
    return(paste(yrs, strftime(x, format = "W%V")))
  }
  year_sym <- "%Y"
  if (grepl("%y", format)) {
    yrs <- sprintf("%02d", yrs %% 100)
    year_sym <- "%y"
  } else if (grepl("%C", format)) {
    yrs <- yrs %/% 100
    year_sym <- "%C"
  }
  wk <- strftime(x, format = "%V")
  wk_sub <- map_chr(wk, ~ gsub("%V", ., x = format))
  year_sub <- map2_chr(yrs, wk_sub, ~ gsub(year_sym, .x, x = .y))
  year_sub
}

#' @rdname period
#' @param year A vector of years.
#' @return `TRUE`/`FALSE` if the year has 53 ISO weeks.
#' @export
#' @examples
#' is_53weeks(2015:2016)
is_53weeks <- function(year) {
  if (is_empty(year)) return(FALSE)
  if (!is_integerish(year) || any(year < 1)) {
    abort("Argument `year` must be positive integers.")
  }
  pre_year <- year - 1
  p_year <- function(year) {
    (year + floor(year / 4) - floor(year / 100) + floor(year / 400)) %% 7
  }
  p_year(year) == 4 | p_year(pre_year) == 3
}

#' @export
print.yearweek <- function(x, format = "%Y W%V", ...) {
  print(format(x, format = format))
  invisible(x)
}

obj_sum.yearweek <- function(x) {
  rep("week", length(x))
}

is_vector_s3.yearweek <- function(x) {
  TRUE
}

pillar_shaft.yearweek <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "right", min_width = 10)
}

#' @rdname period
#' @export
yearmonth <- function(x) {
  UseMethod("yearmonth")
}

as_yearmonth <- function(x) {
  structure(x, tz = NULL, class = c("yearmonth", "Date"))
}

#' @export
c.yearmonth <- function(..., recursive = FALSE) {
  as_yearmonth(NextMethod())
}

#' @export
rep.yearmonth <- function(x, ...) {
  as_yearmonth(NextMethod())
}

#' @export
unique.yearmonth <- function(x, incomparables = FALSE, ...) {
  as_yearmonth(NextMethod())
}

#' @export
diff.yearmonth <- function(x, lag = 1, differences = 1, ...) {
  out <- diff((lubridate::year(x) - 1970) * 12 + lubridate::month(x),
    lag = lag, differences = differences)
  structure(out, class = "difftime", units = "months")
}

#' @export
`+.yearmonth` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  e1_yrmth <- is_yearmonth(e1)
  e2_yrmth <- is_yearmonth(e2)
  if (e1_yrmth && e2_yrmth) {
    abort("Binary `+` is not defined for class yearmonth.")
  }
  if (e1_yrmth) {
    yearmonth(as_date(e1) + lubridate::period(months = e2))
  } else {
    yearmonth(lubridate::period(months = e1) + as_date(e2))
  }
}

#' @export
`-.yearmonth` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  e1_yrmth <- is_yearmonth(e1)
  e2_yrmth <- is_yearmonth(e2)
  if (e1_yrmth && e2_yrmth) {
    res <- units_since(e1) - units_since(e2)
    structure(res, class = "difftime", units = "months")
  } else {
    yearmonth(as_date(e1) - lubridate::period(months = e2, units = "month"))
  }
}

is_yearmonth <- function(x) {
  inherits(x, "yearmonth")
}

#' @export
yearmonth.default <- function(x) {
  dont_know(x, "yearmonth")
}

#' @export
yearmonth.POSIXt <- function(x) {
  as_yearmonth(as_date(lubridate::floor_date(x, unit = "months")))
}

#' @export
yearmonth.Date <- yearmonth.POSIXt

#' @export
yearmonth.character <- function(x) {
  if (is_empty(x)) return(as_yearmonth(x))

  anytime::assertDate(x)
  as_yearmonth(anytime::anydate(x))
}

#' @export
yearmonth.yearweek <- yearmonth.POSIXt

#' @export
yearmonth.yearmonth <- function(x) {
  as_yearmonth(x)
}

#' @export
yearmonth.numeric <- function(x) {
  year <- trunc(x)
  month <- formatC(round((x %% 1) * 12) %% 12 + 1, flag = 0, width = 2)
  result <- lubridate::make_date(year, month, 1)
  as_yearmonth(result)
}

#' @export
yearmonth.yearmon <- yearmonth.numeric

#' @export
format.yearmonth <- function(x, format = "%Y %b", ...) {
  format.Date(x, format = format, ...)
}

#' @export
print.yearmonth <- function(x, format = "%Y %b", ...) {
  print(format(x, format = format, ...))
  invisible(x)
}

obj_sum.yearmonth <- function(x) {
  rep("mth", length(x))
}

is_vector_s3.yearmonth <- is_vector_s3.yearweek

pillar_shaft.yearmonth <- pillar_shaft.yearweek

#' @rdname period
#' @export
yearquarter <- function(x) {
  UseMethod("yearquarter")
}

as_yearquarter <- function(x) {
  structure(x, tz = NULL, class = c("yearquarter", "Date"))
}

#' @export
c.yearquarter <- function(..., recursive = FALSE) {
  as_yearquarter(NextMethod())
}

#' @export
rep.yearquarter <- function(x, ...) {
  as_yearquarter(NextMethod())
}

#' @export
unique.yearquarter <- function(x, incomparables = FALSE, ...) {
  as_yearquarter(NextMethod())
}

#' @export
diff.yearquarter <- function(x, lag = 1, differences = 1, ...) {
  out <- diff((lubridate::year(x) - 1970) * 4 + lubridate::quarter(x),
    lag = lag, differences = differences)
  structure(out, class = "difftime", units = "quarters")
}

#' @export
`+.yearquarter` <- function(e1, e2) {
  if (nargs() == 1L) return(e1)
  e1_yrqtr <- is_yearquarter(e1)
  e2_yrqtr <- is_yearquarter(e2)
  if (e1_yrqtr && e2_yrqtr) {
    abort("Binary `+` is not defined for class yearquarter.")
  }
  if (e1_yrqtr) {
    yearquarter(as_date(e1) + lubridate::period(months = e2 * 3))
  } else {
    yearquarter(lubridate::period(months = e1 * 3) + as_date(e2))
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
    yearquarter(as_date(e1) - lubridate::period(months = e2 * 3))
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
  as_yearquarter(as_date(lubridate::floor_date(x, unit = "quarters")))
}

#' @export
yearquarter.Date <- yearquarter.POSIXt

#' @export
yearquarter.character <- function(x) {
  if (is_empty(x)) return(as_yearquarter(x))

  # exact matching with q, qtr, or quarter
  key_words <- regmatches(x, gregexpr("[[:alpha:]]+", x))
  if (all(grepl("^(q|qtr|quarter)$", key_words, ignore.case = TRUE))) {
    yr_qtr <- regmatches(x, gregexpr("[[:digit:]]+", x))
    digits_lgl <- map_lgl(yr_qtr, ~ !has_length(.x, 2))
    digits_len <- map_int(yr_qtr, ~ sum(nchar(.x)))
    if (any(digits_lgl) || any_not_equal_to_c(digits_len, 5)) {
      abort("Character strings are not in a standard unambiguous format.")
    }
    yr_lgl <- map(yr_qtr, ~ grepl("[[:digit:]]{4}", .x))
    yr <- as.integer(map2_chr(yr_qtr, yr_lgl, ~ .x[.y]))
    qtr <- as.integer(map2_chr(yr_qtr, yr_lgl, ~ .x[!.y]))
    if (any(qtr > 4)) {
      abort("Quarters can't be greater than 4.")
    }
    as_yearquarter(lubridate::make_date(yr, qtr * 3))
  } else {
    anytime::assertDate(x)
    as_yearquarter(anytime::anydate(x))
  }
}

#' @export
yearquarter.yearweek <- yearquarter.POSIXt

#' @export
yearquarter.yearmonth <- yearquarter.POSIXt

#' @export
yearquarter.yearquarter <- function(x) {
  as_yearquarter(x)
}

#' @export
yearquarter.numeric <- function(x) {
  year <- trunc(x)
  last_month <- trunc((x %% 1) * 4 + 1) * 3
  first_month <- formatC(last_month - 2, flag = 0, width = 2)
  result <- lubridate::make_date(year, first_month, 1)
  as_yearquarter(result)
}

#' @export
yearquarter.yearqtr <- yearquarter.numeric

#' @importFrom lubridate as_date
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
  year <- lubridate::year(x)
  year_sym <- "%Y"
  if (grepl("%y", format)) {
    year <- sprintf("%02d", year %% 100)
    year_sym <- "%y"
  } else if (grepl("%C", format)) {
    year <- year %/% 100
    year_sym <- "%C"
  }
  qtr <- lubridate::quarter(x)
  qtr_sub <- map_chr(qtr, ~ gsub("%q", ., x = format))
  year_sub <- map2_chr(year, qtr_sub, ~ gsub(year_sym, .x, x = .y))
  year_sub
}

#' @export
print.yearquarter <- function(x, format = "%Y Q%q", ...) {
  print(format(x, format = format))
  invisible(x)
}

obj_sum.yearquarter <- function(x) {
  rep("qtr", length(x))
}

is_vector_s3.yearquarter <- is_vector_s3.yearweek

pillar_shaft.yearquarter <- pillar_shaft.yearweek

#' @export
seq.yearweek <- function(
  from, to, by, length.out = NULL, along.with = NULL,
  ...) {
  bad_by(by)
  by_wk <- paste(by, "week")
  yearweek(seq_date(
    from = from, to = to, by = by_wk, length.out = length.out,
    along.with = along.with, ...
  ))
}

#' @export
seq.yearmonth <- function(
  from, to, by, length.out = NULL, along.with = NULL,
  ...) {
  bad_by(by)
  by_mth <- paste(by, "month")
  yearmonth(seq_date(
    from = from, to = to, by = by_mth, length.out = length.out,
    along.with = along.with, ...
  ))
}

#' @export
seq.yearquarter <- function(
  from, to, by, length.out = NULL, along.with = NULL,
  ...) {
  bad_by(by)
  by_qtr <- paste(by, "quarter")
  yearquarter(seq_date(
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
  yearweek(NextMethod())
}

#' @export
`[.yearmonth` <- function(x, ..., drop = TRUE) {
  yearmonth(NextMethod())
}

#' @export
`[.yearquarter` <- function(x, ..., drop = TRUE) {
  yearquarter(NextMethod())
}

#' @export
as.POSIXlt.yearquarter <- function(x, tz = "", ...) {
  as.POSIXlt(as_date(x), tz = tz, ...)
}

#' Time units since Unix Epoch
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
  as.numeric((lubridate::year(x) - 1970) * 12 + lubridate::month(x) - 1)
}

#' @export
units_since.yearquarter <- function(x) {
  as.numeric((lubridate::year(x) - 1970) * 4 + lubridate::quarter(x) - 1)
}

#' @export
units_since.Date <- function(x) {
  as.numeric(x)
}

#' @export
units_since.POSIXct <- function(x) {
  as.numeric(x)
}

seq_date <- function(
  from, to, by, length.out = NULL, along.with = NULL,
  ...) {
  if (missing(from))
    stop("'from' must be specified")
  if (!inherits(from, "Date"))
    stop("'from' must be a \"Date\" object")
  if (length(as.Date(from)) != 1L)
    stop("'from' must be of length 1")
  if (!missing(to)) {
    if (!inherits(to, "Date"))
      stop("'to' must be a \"Date\" object")
    if (length(as.Date(to)) != 1L)
      stop("'to' must be of length 1")
  }
  if (!is.null(along.with)) { # !missing(along.with) in seq.Date
    length.out <- length(along.with)
  } else if (!is.null(length.out)) {
    if (length(length.out) != 1L)
      stop("'length.out' must be of length 1")
    length.out <- ceiling(length.out)
  }
  status <- c(!missing(to), !missing(by), !is.null(length.out))
  if (sum(status) != 2L)
    stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
  if (missing(by)) {
    from <- unclass(as.Date(from))
    to <- unclass(as.Date(to))
    res <- seq.int(from, to, length.out = length.out)
    return(structure(res, class = "Date"))
  }
  if (length(by) != 1L)
    stop("'by' must be of length 1")
  valid <- 0L
  if (inherits(by, "difftime")) {
    by <- switch(attr(by, "units"), secs = 1/86400, mins = 1/1440,
        hours = 1/24, days = 1, weeks = 7) * unclass(by)
  } else if (is.character(by)) {
    by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
    if (length(by2) > 2L || length(by2) < 1L)
      stop("invalid 'by' string")
    valid <- pmatch(by2[length(by2)], c("days", "weeks",
      "months", "quarters", "years"))
    if (is.na(valid))
      stop("invalid string for 'by'")
    if (valid <= 2L) {
      by <- c(1, 7)[valid]
      if (length(by2) == 2L)
        by <- by * as.integer(by2[1L])
    } else by <- if (length(by2) == 2L)
      as.integer(by2[1L])
    else 1
  } else if (!is.numeric(by))
    stop("invalid mode for 'by'")
  if (is.na(by))
    stop("'by' is NA")
  if (valid <= 2L) {
    from <- unclass(as.Date(from))
    if (!is.null(length.out))
      res <- seq.int(from, by = by, length.out = length.out)
    else {
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
      if (valid == 4L)
        by <- by * 3
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
    res <- if (by > 0)
      res[res <= to]
    else res[res >= to]
  }
  res
}

bad_by <- function(by) {
  if (!is_bare_numeric(by, n = 1)) {
    abort("`by` only takes a numeric.")
  }
}
