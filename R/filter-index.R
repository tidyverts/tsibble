filter_index <- function(.data, ...) {
  
}

#' @importFrom stats start end
#' @importFrom lubridate tz<-
start.Date <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    min(x)
  } else {
    anytime::assertDate(y)
    tz <- lubridate::tz(x)
    y <- anytime::anydate(y)
    tz(y) <- tz
    y
  }
}

end.Date <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + lubridate::period(1, "day")
  } else {
    anytime::assertDate(y)
    
    lgl_yrmth <- nchar(y) < 8 & nchar(y) > 4
    lgl_yr <- nchar(y) < 5
    tz <- lubridate::tz(x)
    y <- anytime::anydate(y)
    tz(y) <- tz
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
    anytime::assertTime(y)
    tz <- lubridate::tz(x)
    y <- anytime::anytime(y)
    tz(y) <- tz
    y
  }
}

end.POSIXct <- function(x, y = NULL, ...) {
  if (is_null(y)) {
    max(x) + lubridate::period(1, "hour")
  } else {
    anytime::assertTime(y)
    
    lgl_yrmth <- nchar(y) < 8 & nchar(y) > 4
    lgl_yr <- nchar(y) < 5
    tz <- lubridate::tz(x)
    y <- anytime::anytime(y)
    tz(y) <- tz
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
    y[lgl_date] <- y[lgl_date] + lubridate::period(1, "hour")
    y
  }
}
