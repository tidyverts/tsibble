#' A shorthand for filtering time index for a tsibble
#'
#' This shorthand respects time zone and encourages compact expressions.
#'
#' @param .data A tsibble.
#' @param ... Formulas that specify start and end periods (inclusive) or strings.
#' * `~ end` or `. ~ end`: from the very beginning to a specified ending period.
#' * `start ~ end`: from specified beginning to ending periods.
#' * `start ~ .`: from a specified beginning to the very end of the data.
#' Supported index type: `POSIXct` (to seconds), `Date`, `yearweek`, `yearmonth`/`yearmon`,
#' `yearquarter`/`yearqtr`, `hms`/`difftime` & `numeric`.
#'
#' @section System Time Zone ("Europe/London"): 
#' There is a known issue of an extra hour gained for a machine setting time 
#' zone to "Europe/London", regardless of the time zone associated with
#' the POSIXct inputs. It relates to *anytime* and *Boost*. Use `Sys.timezone()` 
#' to check if the system time zone is "Europe/London". I would recommend to
#' change the global environment "TZ" to other equivalent names: GB, GB-Eire, 
#' Europe/Belfast, Europe/Guernsey, Europe/Isle_of_Man and Europe/Jersey as
#' documented in `?Sys.timezone()`, using `Sys.setenv(TZ)`.
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
  idx <- index(.data)
  filter(.data, time_in(!! idx, ...))
}

#' If time falls in the ranges using compact expressions
#'
#' This function respects time zone and encourages compact expressions.
#'
#' @param x A vector of time index, such as classes `POSIXct`, `Date`, `yearweek`, 
#' `yearmonth`, `yearquarter`, `hms`/`difftime`, and `numeric`.
#' @inheritParams filter_index
#'
#' @inheritSection filter_index System Time Zone ("Europe/London")
#' @return logical vector
#' @seealso [filter_index] for filtering tsibble
#' @export
#' @examples
#' x <- unique(pedestrian$Date_Time)
#' lgl <- time_in(x, ~ "2015-02", "2015-08" ~ "2015-09", "2015-12" ~ "2016-02")
#' lgl[1:10]
#' # more specific
#' lgl2 <- time_in(x, "2015-03-23 10" ~ "2015-10-31 12")
#' lgl2[1:10]
#'
#' pedestrian %>% 
#'   filter(time_in(Date_Time, "2015-03-23 10" ~ "2015-10-31 12"))
#' pedestrian %>% 
#'   filter(time_in(Date_Time, "2015")) %>% 
#'   mutate(Season = ifelse(
#'     time_in(Date_Time, "2015-03" ~ "2015-08"),
#'     "Autumn-Winter", "Spring-Summer"
#'   ))
time_in <- function(x, ...) {
  UseMethod("time_in")
}

#' @export
time_in.default <- function(x, ...) {
  dont_know(x, "time_in")
}

#' @export
time_in.POSIXct <- function(x, ...) {
  formulas <- list2(...)
  n <- length(formulas)
  if (n == 0) return(!logical(length(x)))

  local_tz <- Sys.timezone()
  if ("Europe/London" %in% local_tz) {
    warn("System time zone: \"Europe/London\".\nIt may yield an unexpected output. Please see `?filter_index` for details.")
  }

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

#' @export
time_in.Date <- time_in.POSIXct

#' @export
time_in.difftime <- time_in.POSIXct

#' @export
time_in.yearweek <- time_in.POSIXct

#' @export
time_in.yearmonth <- time_in.POSIXct

#' @export
time_in.yearmon <- time_in.POSIXct

#' @export
time_in.yearquarter <- time_in.POSIXct

#' @export
time_in.yearqtr <- time_in.POSIXct

#' @export
time_in.numeric <- time_in.POSIXct

#' @importFrom stats start end
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
    attr(y, "tzone") <- NULL
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
    attr(y, "tzone") <- NULL
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
    lubridate::force_tz(y, lubridate::tz(x), roll = TRUE)
  }
}

end.POSIXct <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + lubridate::period(1, "second")
  } else {
    abort_not_chr(y, class = "POSIXct")
    anytime::assertTime(y)
    
    lgl_date <- nchar(y) > 7 & nchar(y) < 11
    lgl_yrmth <- nchar(y) < 9 & nchar(y) > 4
    lgl_yr <- nchar(y) < 5
    y <- anytime::utctime(y, tz = "UTC")
    y <- lubridate::force_tz(y, lubridate::tz(x), roll = TRUE)
    if (any(lgl_date)) {
      y[lgl_date] <- y[lgl_date] + lubridate::period(1, "day")
    }
    if (any(lgl_yrmth)) {
      y[lgl_yrmth] <- lubridate::rollback(
        y[lgl_yrmth] + lubridate::period(1, "month"), 
        roll_to_first = TRUE
      )
    }
    if (any(lgl_yr)) {
      y[lgl_yr] <- y[lgl_yr] + lubridate::period(1, "year")
    }
    lgl_time <- !(lgl_date | lgl_yrmth | lgl_yr)
    y[lgl_time] <- y[lgl_time] + 1
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
