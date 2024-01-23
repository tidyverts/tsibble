#' A shorthand for filtering time index for a tsibble
#'
#' @description
#' This shorthand respects time zones and encourages compact expressions.
#'
#' @param .data A tsibble.
#' @param ... Formulas that specify start and end periods (inclusive), or strings.
#' * `~ end` or `. ~ end`: from the very beginning to a specified ending period.
#' * `start ~ end`: from specified beginning to ending periods.
#' * `start ~ .`: from a specified beginning to the very end of the data.
#' Supported index type: `POSIXct` (to seconds), `Date`, `yearweek`, `yearmonth`/`yearmon`,
#' `yearquarter`/`yearqtr`, `hms`/`difftime` & `numeric`.
#' @inheritParams dplyr::filter
#'
#' @section System Time Zone ("Europe/London"):
#' There is a known issue of an extra hour gained for a machine setting time
#' zone to "Europe/London", regardless of the time zone associated with
#' the POSIXct inputs. It relates to *anytime* and *Boost*. Use `Sys.timezone()`
#' to check if the system time zone is "Europe/London". It would be recommended to
#' change the global environment "TZ" to other equivalent names: GB, GB-Eire,
#' Europe/Belfast, Europe/Guernsey, Europe/Isle_of_Man and Europe/Jersey as
#' documented in `?Sys.timezone()`, using `Sys.setenv(TZ = "GB")` for example.
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
#'   filter_index(~"2015-02", "2015-08" ~ "2015-09", "2015-12" ~ "2016-02")
#'
#' # entire 2015
#' pedestrian %>%
#'   filter_index("2015")
#'
#' # specific
#' pedestrian %>%
#'   filter_index("2015-03-23" ~ "2015-10")
#' pedestrian %>%
#'   filter_index("2015-03-23" ~ "2015-10-31")
#' pedestrian %>%
#'   filter_index("2015-03-23 10" ~ "2015-10-31 12")
filter_index <- function(.data, ..., .preserve = FALSE) {
  idx <- index(.data)
  filter(.data, time_in(!!idx, ...), .preserve = .preserve)
}

#' If time falls in the ranges using compact expressions
#'
#' @description
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
#' lgl <- time_in(x, ~"2015-02", "2015-08" ~ "2015-09", "2015-12" ~ "2016-02")
#' lgl[1:10]
#' # more specific
#' lgl2 <- time_in(x, "2015-03-23 10" ~ "2015-10-31 12")
#' lgl2[1:10]
#'
#' library(dplyr)
#' pedestrian %>%
#'   filter(time_in(Date_Time, "2015-03-23 10" ~ "2015-10-31 12"))
#' pedestrian %>%
#'   filter(time_in(Date_Time, "2015")) %>%
#'   mutate(Season = ifelse(
#'     time_in(Date_Time, "2015-03" ~ "2015-08"),
#'     "Autumn-Winter", "Spring-Summer"
#'   ))
time_in <- function(x, ...) {
  formulas <- list2(...)
  n <- length(formulas)
  if (n == 0) return(!logical(length(x)))

  if (is.POSIXct(x)) {
    local_tz <- Sys.timezone()
    if ("Europe/London" %in% local_tz) {
      warn("System time zone: \"Europe/London\".\nIt may yield an unexpected output. Please see `?filter_index` for details.")
    }
  }

  lgl <- lhs <- rhs <- vec_init(list(), n)
  for (i in seq_len(n)) {
    f <- formulas[[i]]
    if (is_atomic(f)) {
      f <- new_formula(f, f)
    }
    env <- f_env(f)
    lhs[[i]] <- start_window(x, eval_bare(is_dot_null(f_lhs(f)), env = env))
    rhs[[i]] <- end_window(x, eval_bare(is_dot_null(f_rhs(f)), env = env))
    lgl[[i]] <- eval_bare(x >= lhs[[i]] & x < rhs[[i]])
  }

  reduce(lgl, `|`)
}

start_window <- function(x, y = NULL, ...) {
  UseMethod("start_window")
}

end_window <- function(x, y = NULL, ...) {
  UseMethod("end_window")
}

start_window.numeric <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    min(x)
  } else {
    as.numeric(y)
  }
}

end_window.numeric <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + 1
  } else {
    as.numeric(y) + 1
  }
}

start_window.difftime <- function(x, y = NULL, ...) {
  if (!requireNamespace("hms", quietly = TRUE)) {
    abort("Package `hms` required.\nPlease install and try again.")
  }
  if (is_null(y)) {
    hms::as_hms(min(x))
  } else {
    abort_not_chr(y, class = "hms/difftime")
    assert_difftime(y)
  }
}

end_window.difftime <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    hms::as_hms(max(x) + 1)
  } else {
    abort_not_chr(y, class = "hms/difftime")
    y <- assert_difftime(y)
    hms::as_hms(y + 1)
  }
}

start_window.Date <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    min(x)
  } else {
    abort_not_chr(y, class = "Date")
    assertDate(y)
    y <- utcdate(y, tz = "UTC")
    attr(y, "tzone") <- NULL
    y
  }
}

end_window.Date <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + period(1, "day")
  } else {
    abort_not_chr(y, class = "Date")
    assertDate(y)

    lgl_yrmth <- nchar(y) < 8 & nchar(y) > 4
    lgl_yr <- nchar(y) < 5
    y <- utcdate(y, tz = "UTC")
    attr(y, "tzone") <- NULL
    if (any(lgl_yrmth)) {
      y[lgl_yrmth] <- rollback(
        y[lgl_yrmth] + period(1, "month"),
        roll_to_first = TRUE
      )
    }
    if (any(lgl_yr)) {
      y[lgl_yr] <- y[lgl_yr] + period(1, "year")
    }
    lgl_date <- !(lgl_yrmth | lgl_yr)
    y[lgl_date] <- y[lgl_date] + period(1, "day")
    y
  }
}

start_window.POSIXct <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    min(x)
  } else {
    abort_not_chr(y, class = "POSIXct")
    assertTime(y)
    y <- utctime(y, tz = "UTC")
    force_tz(y, tz(x), roll_dst = c("boundary", "post"))
  }
}

end_window.POSIXct <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + period(1, "second")
  } else {
    abort_not_chr(y, class = "POSIXct")
    assertTime(y)

    lgl_date <- nchar(y) > 7 & nchar(y) < 11
    lgl_yrmth <- nchar(y) < 9 & nchar(y) > 4
    lgl_yr <- nchar(y) < 5
    y <- utctime(y, tz = "UTC")
    y <- force_tz(y, tz(x), roll_dst = c("boundary", "post"))
    if (any(lgl_date)) {
      y[lgl_date] <- y[lgl_date] + period(1, "day")
    }
    if (any(lgl_yrmth)) {
      y[lgl_yrmth] <- rollback(
        y[lgl_yrmth] + period(1, "month"),
        roll_to_first = TRUE
      )
    }
    if (any(lgl_yr)) {
      y[lgl_yr] <- y[lgl_yr] + period(1, "year")
    }
    lgl_time <- !(lgl_date | lgl_yrmth | lgl_yr)
    y[lgl_time] <- y[lgl_time] + 1
    y
  }
}

start_window.yearweek <- function(x, y = NULL, ...) {
  wk_start <- week_start(x)
  x <- as_date(x)
  if (!is_null(y)) {
    abort_not_chr(y, class = "yearweek")
    y <- as.character(as_date(yearweek(y, week_start = wk_start)))
  }
  yearweek(start_window(x = x, y = y), week_start = wk_start)
}

end_window.yearweek <- function(x, y = NULL, ...) {
  wk_start <- week_start(x)
  x <- as_date(x)
  if (!is_null(y)) {
    abort_not_chr(y, class = "yearweek")
    y <- as.character(as_date(yearweek(y, week_start = wk_start)))
  }
  yearweek(end_window(x = x, y = y), week_start = wk_start) + 1
}

start_window.yearmonth <- function(x, y = NULL, ...) {
  x <- as_date(x)
  if (!is_null(y)) {
    abort_not_chr(y, class = "yearmonth")
    y <- as.character(as_date(yearmonth(y)))
  }
  yearmonth(start_window(x = x, y = y))
}

end_window.yearmonth <- function(x, y = NULL, ...) {
  x <- as_date(x)
  if (!is_null(y)) {
    abort_not_chr(y, class = "yearmonth")
    y <- as.character(as_date(yearmonth(y)))
  }
  yearmonth(end_window(x = x, y = y)) + 1
}

start_window.yearquarter <- function(x, y = NULL, ...) {
  x <- as_date(x)
  if (!is_null(y)) {
    abort_not_chr(y, class = "yearquarter")
    y <- as.character(as_date(yearquarter(y)))
  }
  yearquarter(start_window(x = x, y = y))
}

end_window.yearquarter <- function(x, y = NULL, ...) {
  x <- as_date(x)
  if (!is_null(y)) {
    abort_not_chr(y, class = "yearquarter")
    y <- as.character(as_date(yearquarter(y)))
  }
  yearquarter(end_window(x = x, y = y)) + 1
}

start_window.yearmon <- function(x, y = NULL, ...) {
  x <- yearmonth(x)
  start_window(x, y = y)
}

end_window.yearmon <- function(x, y = NULL, ...) {
  x <- yearmonth(x)
  end_window(x, y = y)
}

start_window.yearqtr <- function(x, y = NULL, ...) {
  x <- yearquarter(x)
  start_window(x, y = y)
}

end_window.yearqtr <- function(x, y = NULL, ...) {
  x <- yearquarter(x)
  end_window(x, y = y)
}

is_dot_null <- function(x) { # x is a sym
  if (is_null(x) || x == sym(".")) {
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
  res <- hms::as_hms(x)
  if (is.na(res)) {
    abort(sprintf("Input data '%s' cannot be expressed as difftime type.", x))
  }
  res
}
