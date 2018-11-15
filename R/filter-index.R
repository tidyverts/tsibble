#' A shorthand for filtering for time index
#'
#' This shorthand takes care of time zone and only subsets time windows.
#'
#' @param .data A tsibble.
#' @param ... A list of formulas that specify start and end periods (inclusive).
#' * `~ end` or `. ~ end`: from the very beginning to a specified ending period.
#' * `start ~ end`: from specified beginning to ending periods.
#' * `start ~ .`: from a specified beginning to the very end of the data.
#' Supported index type: `POSIXct` (to seconds), `Date`, `yearweek`, `yearmonth`/`yearmon`,
#' `yearquarter`/`yearqtr`, `hms`/`difftime` & `numeric`.
#'
#' @seealso [time_in] for a vector of time index
#' @export
#' @examples
#' # from the starting time to the end of Feb, 2015
#' pedestrian %>%
#'   filter_index(~ "2015-02")
#'
#' # entire Feb 2015, & from the beginning of Aug 2016 to the end
#' pedestrian %>%
#'   filter_index("2015-02", "2016-08" ~ .)
#'
#' # multiple time windows
#' pedestrian %>%
#'   filter_index(~ "2015-02", "2015-08" ~ "2015-09", "2015-12" ~ "2016-02")
#'
#' # entire 2015
#' pedestrian %>%
#'   filter_index(~ "2015")
#'
#' # specific
#' pedestrian %>% 
#'   filter_index("2015-03-23" ~ "2015-10")
#' pedestrian %>% 
#'   filter_index("2015-03-23" ~ "2015-10-31")
#' pedestrian %>% 
#'   filter_index("2015-03-23 10" ~ "2015-10-31 12")
filter_index <- function(.data, ...) {
  UseMethod("filter_index")
}

#' @export
filter_index.tbl_ts <- function(.data, ...) {
  index <- eval_tidy(index(.data), data = .data)
  lgl <- time_in(index, ...)
  filter(.data, lgl)
}

#' If time falls in the ranges, with no need for time zone specification
#'
#' @param x A vector of time index, such as classes `POSIXct`, `Date`, `yearweek`, 
#' `yearmonth`, `yearquarter`, `hms`/`difftime`, and `numeric`.
#' @inheritParams filter_index
#'
#' @return logical vector
#' @seealso [filter_index] for filtering tsibble
#' @export
#' @examples
#' x <- unique(pedestrian$Date_Time)
#' lgl <- time_in(x, ~ "2015-02", "2015-08" ~ "2015-09", "2015-12" ~ "2016-02")
#' lgl[1:10]
time_in <- function(x, ...) {
  UseMethod("time_in")
}

#' @export
time_in.default <- function(x, ...) {
  formulas <- list2(...)
  n <- length(formulas)
  if (n == 0) return(!logical(length(x)))

  lgl <- lhs <- rhs <- vector("list", n)
  for (i in seq_len(n)) {
    f <- formulas[[i]]
    if (is_atomic(f)) {
      f <- new_formula(f, f)
    }
    env <- f_env(f)
    lhs[[i]] <- start(x, eval_bare(is_dot_null(f_lhs(f)), env = env))
    rhs[[i]] <- end(x, eval_bare(is_dot_null(f_rhs(f)), env = env))
    lgl[[i]] <- eval_bare(x >= lhs[[i]] & x < rhs[[i]])
  }

  purrr::reduce(lgl, `|`)
}

#' @importFrom stats start end
#' @importFrom lubridate tz<-
start.numeric <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    min(x)
  } else {
    as.numeric(y)
  }
}

end.numeric <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + 1
  } else {
    as.numeric(y) + 1
  }
}

start.difftime <- function(x, y = NULL, ...) {
  if (!requireNamespace("hms", quietly = TRUE)) {
    abort("Package `hms` required.\nPlease install and try again.")
  }
  if (is_null(y)) {
    hms::as.hms(min(x))
  } else {
    abort_not_chr(y, class = "hms/difftime")
    assert_difftime(y)
  }
}

end.difftime <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    hms::as.hms(max(x) + 1)
  } else {
    abort_not_chr(y, class = "hms/difftime")
    y <- assert_difftime(y)
    hms::as.hms(y + 1)
  }
}

start.Date <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    min(x)
  } else {
    abort_not_chr(y, class = "Date")
    anytime::assertDate(y)
    y <- anytime::utcdate(y, tz = "UTC")
    tz(y) <- lubridate::tz(x)
    y
  }
}

end.Date <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + lubridate::period(1, "day")
  } else {
    abort_not_chr(y, class = "Date")
    anytime::assertDate(y)
    
    lgl_yrmth <- nchar(y) < 8 & nchar(y) > 4
    lgl_yr <- nchar(y) < 5
    y <- anytime::utcdate(y, tz = "UTC")
    tz(y) <- lubridate::tz(x)
    if (any(lgl_yrmth)) {
      y[lgl_yrmth] <- lubridate::rollback(
        y[lgl_yrmth] + lubridate::period(1, "month"), 
        roll_to_first = TRUE
      )
    }
    if (any(lgl_yr)) {
      y[lgl_yr] <- y[lgl_yr] + lubridate::period(1, "year")
    }
    lgl_date <- !(lgl_yrmth | lgl_yr)
    y[lgl_date] <- y[lgl_date] + lubridate::period(1, "day")
    y
  }
}

start.POSIXct <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    min(x)
  } else {
    abort_not_chr(y, class = "POSIXct")
    anytime::assertTime(y)
    y <- anytime::utctime(y, tz = "UTC")
    tz(y) <- lubridate::tz(x)
    y
  }
}

end.POSIXct <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + lubridate::period(1, "second")
  } else {
    abort_not_chr(y, class = "POSIXct")
    anytime::assertTime(y)
    
    lgl_yrmth <- nchar(y) < 8 & nchar(y) > 4
    lgl_yr <- nchar(y) < 5
    y <- anytime::utctime(y, tz = "UTC")
    tz(y) <- lubridate::tz(x)
    if (any(lgl_yrmth)) {
      y[lgl_yrmth] <- lubridate::rollback(
        y[lgl_yrmth] + lubridate::period(1, "month"), 
        roll_to_first = TRUE
      )
    }
    if (any(lgl_yr)) {
      y[lgl_yr] <- y[lgl_yr] + lubridate::period(1, "year")
    }
    lgl_date <- !(lgl_yrmth | lgl_yr)
    y[lgl_date] <- y[lgl_date] + lubridate::period(1, "second")
    y
  }
}

start.yearweek <- function(x, y = NULL, ...) {
  x <- as_date(x)
  NextMethod()
}

end.yearweek <- function(x, y = NULL, ...) {
  x <- as_date(x)
  NextMethod()
}

start.yearmonth <- start.yearweek

end.yearmonth <- end.yearweek

start.yearquarter <- start.yearweek

end.yearquarter <- end.yearweek

start.yearmon <- function(x, y = NULL, ...) {
  x <- yearmonth(x)
  start(x, y = y)
}

end.yearmon <- function(x, y = NULL, ...) {
  x <- yearmonth(x)
  end(x, y = y)
}

start.yearqtr <- function(x, y = NULL, ...) {
  x <- yearquarter(x)
  start(x, y = y)
}

end.yearqtr <- function(x, y = NULL, ...) {
  x <- yearquarter(x)
  end(x, y = y)
}

is_dot_null <- function(x) { # x is a sym
  if (is_null(x)) {
    NULL
  } else if (x == sym(".")) {
    NULL
  } else {
    x
  }
}

abort_not_chr <- function(x, class) {
  if (!is_character(x)) {
    abort(sprintf("Must be character(s) for %s, not %s.", class, typeof(x)))
  }
}

assert_difftime <- function(x) {
  res <- hms::as.hms(x)
  if (is.na(res)) {
    abort(sprintf("Input data '%s' cannot be expressed as difftime type.", x))
  }
  res
}
