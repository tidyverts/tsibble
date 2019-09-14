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
