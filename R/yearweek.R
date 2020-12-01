#' Represent year-week based on the ISO 8601 standard (with flexible start day)
#'
#' @description
#' \lifecycle{stable}
#'
#' Create or coerce using `yearweek()`.
#'
#' @inheritSection yearmonth Display
#'
#' @inheritParams yearmonth
#' @param week_start An integer between 1 (Monday) and 7 (Sunday) to specify
#' the day on which week starts following ISO conventions. Default to 1 (Monday).
#' Use `options(lubridate.week.start = 7)` to set this parameter globally.
#'
#' @return year-week (`yearweek`) objects.
#'
#' @seealso [scale_x_yearweek] and others for ggplot2 scales
#' @family index functions
#' @rdname year-week
#' @export
#' @examples
#' # coerce POSIXct/Dates to yearweek
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 week")
#' yearweek(x)
#' yearweek(x, week_start = 7)
#'
#' # parse characters
#' yearweek(c("2018 W01", "2018 Wk01", "2018 Week 1"))
#'
#' # seq() and arithmetic
#' wk1 <- yearweek("2017 W50")
#' wk2 <- yearweek("2018 W12")
#' seq(from = wk1, to = wk2, by = 2)
#' wk1 + 0:9
#'
#' # display formats
#' format(c(wk1, wk2), format = "%V/%Y")
yearweek <- function(x, week_start = getOption("lubridate.week.start", 1)) {
  UseMethod("yearweek")
}

#' @export
yearweek.default <- function(x,
                             week_start = getOption("lubridate.week.start", 1)) {
  dont_know(x, "yearweek")
}

#' @export
yearweek.NULL <- function(x,
                          week_start = getOption("lubridate.week.start", 1)) {
  new_yearweek(double(), week_start = week_start)
}

#' @export
yearweek.numeric <- function(x,
                             week_start = getOption("lubridate.week.start", 1)) {
  # anchor to "1970 W01" regardless of dates
  new_yearweek(new_date(-4 + week_start) + x * 7, week_start)
}

#' @export
yearweek.POSIXct <- function(x,
                             week_start = getOption("lubridate.week.start", 1)) {
  new_yearweek(floor_date(as_date(x), unit = "weeks", week_start = week_start),
    week_start = week_start)
}

#' @export
yearweek.POSIXlt <- yearweek.POSIXct

#' @export
yearweek.Date <- yearweek.POSIXct

#' @export
yearweek.character <- function(x,
                               week_start = getOption("lubridate.week.start", 1)) {
  key_words <- regmatches(x, gregexpr("[[:alpha:]]+", x))
  if (all(grepl("^(w|wk|week)$", key_words, ignore.case = TRUE))) {
    yr_week <- regmatches(x, gregexpr("[[:digit:]]+", x))
    digits_lgl <- map_lgl(yr_week, function(.x) !has_length(.x, 2))
    digits_len <- map_int(yr_week, function(.x) sum(nchar(.x)))
    if (any(digits_lgl) || any(digits_len < 5)) {
      abort("Character strings are not in a standard unambiguous format.")
    }
    yr_lgl <- map(yr_week, function(.x) grepl("[[:digit:]]{4}", .x))
    yr <- as.integer(map2_chr(yr_week, yr_lgl, function(.x, .y) .x[.y]))
    week <- as.integer(map2_chr(yr_week, yr_lgl, function(.x, .y) .x[!.y]))
    if (any(week > 53)) {
      abort("Weeks can't be greater than 53.")
    }
    check_53 <- !is_53weeks(yr, week_start) & (week > 52)
    if (any(check_53)) {
      abort(sprintf("Year %s can't be 53 weeks.", comma(yr[check_53])))
    }
    convert_week_to_date(yr, week, week_start)
  } else {
    assertDate(x)
    yearweek(anydate(x), week_start)
  }
}


#' @export
yearweek.yearweek <- function(x, week_start = attr(x, "week_start")) {
  new_yearweek(new_date(x) + week_start - week_start(x), week_start)
}

new_yearweek <- function(x = double(), week_start = 1) {
  if (!has_length(week_start, 1)) {
    abort("`week_start` must be of length 1.")
  }
  if (week_start < 1 || week_start > 7) {
    abort("`week_start` only accepts a value between 1 and 7.")
  }
  new_vctr(x, week_start = week_start, class = "yearweek")
}

week_start <- function(x) {
  attr(x, "week_start") %||% 1 # avoid breaking changes
}

#' @rdname year-week
#' @export
is_yearweek <- function(x) {
  inherits(x, "yearweek")
}

#' @export
is.numeric.yearweek <- function(x) {
  FALSE
}

#' @export
tz.yearweek <- function(x) {
  "UTC"
}

# diff.yearweek <- function(x, lag = 1, differences = 1, ...) {
#   out <- diff((as_date(x) - as_date("1969-12-29")) / 7,
#     lag = lag, differences = differences
#   )
#   structure(out, class = "difftime", units = "weeks")
# }

#' @rdname tsibble-vctrs
#' @export
vec_cast.yearweek <- function(x, to, ...) {
  UseMethod("vec_cast.yearweek")
}

#' @export
vec_cast.Date.yearweek <- function(x, to, ...) {
  new_date(x)
}

#' @export
vec_cast.POSIXct.yearweek <- function(x, to, ...) {
  as.POSIXct(new_date(x), ...)
}

#' @export
vec_cast.double.yearweek <- function(x, to, ...) {
  as.double((as_date(x) - as_date("1969-12-29") - week_start(x) + 1) / 7)
}

#' @export
vec_cast.POSIXlt.yearweek <- function(x, to, ...) {
  as.POSIXlt(new_date(x), ...)
}

#' @export
vec_cast.yearweek.yearweek <- function(x, to, ...) {
  yearweek(x, week_start(to))
}

#' @export
vec_cast.character.yearweek <- function(x, to, ...) {
  format(x)
}

#' @rdname tsibble-vctrs
#' @export
vec_ptype2.yearweek <- function(x, y, ...) {
  UseMethod("vec_ptype2.yearweek", y)
}

#' @export
vec_ptype2.yearweek.POSIXct <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.POSIXct.yearweek <- function(x, y, ...) {
  new_datetime()
}

#' @export
vec_ptype2.yearweek.Date <- function(x, y, ...) {
  new_date()
}

#' @export
vec_ptype2.yearweek.yearweek <- function(x, y, ...) {
  if (week_start(x) != week_start(y)) {
    abort("Can't combine <yearweek> with different `week_start`.")
  }
  new_yearweek()
}

#' @export
vec_ptype2.Date.yearweek <- function(x, y, ...) {
  new_date()
}

#' @rdname tsibble-vctrs
#' @method vec_arith yearweek
#' @export
vec_arith.yearweek <- function(op, x, y, ...) {
  UseMethod("vec_arith.yearweek", y)
}

#' @method vec_arith.yearweek default
#' @export
vec_arith.yearweek.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @method vec_arith.yearweek numeric
#' @export
vec_arith.yearweek.numeric <- function(op, x, y, ...) {
  if (op == "+") {
    new_yearweek(as_date(x) + y * 7, week_start(x))
  } else if (op == "-") {
    new_yearweek(as_date(x) - y * 7, week_start(x))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.numeric yearweek
#' @export
vec_arith.numeric.yearweek <- function(op, x, y, ...) {
  if (op == "+") {
    yearweek(x * 7 + as_date(y), week_start(y))
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yearweek yearweek
#' @export
vec_arith.yearweek.yearweek <- function(op, x, y, ...) {
  if (op == "-") {
    res <- as.double(x) - as.double(y)
    structure(res, class = "difftime", units = "weeks")
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.yearweek MISSING
#' @export
vec_arith.yearweek.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = x,
    `+` = x,
    stop_incompatible_op(op, x, y)
  )
}

#' @export
format.yearweek <- function(x, format = "%Y W%V", ...) {
  wk_start <- week_start(x)
  x <- as_date(x)
  mth <- month(x)
  wk <- extract_week(x, week_start = wk_start)
  shift_year <- period(1, units = "year")
  lgl1 <- mth == 1 & wk == 53
  lgl2 <- mth == 12 & wk == 1
  vec_slice(x, lgl1) <- vec_slice(x, lgl1) - shift_year
  vec_slice(x, lgl2) <- vec_slice(x, lgl2) + shift_year
  wk_chr <- formatC(wk, width = 2, flag = "0")
  wk_sub <- map_chr(wk_chr, function(.x) gsub("%V", .x, x = format))
  wk_sub[is.na(wk_sub)] <- "-"
  format.Date(x, format = wk_sub)
}

#' @export
obj_print_data.yearweek <- function(x, ...) {
  if (length(x) == 0) return()
  print(format(x))
}

#' @export
obj_print_footer.yearweek <- function(x, ...) {
  cat_line("# Week starts on: ", fmt_week(week_start(x)))
}

fmt_week <- function(x) {
  switch(x,
    `1` = "Monday",
    `2` = "Tuesday",
    `3` = "Wednesday",
    `4` = "Thursday",
    `5` = "Friday",
    `6` = "Saturday",
    `7` = "Sunday"
  )
}

#' @export
vec_ptype_abbr.yearweek <- function(x, ...) {
  "week"
}

#' @export
seq.yearweek <- function(from, to, by, length.out = NULL, along.with = NULL,
                         ...) {
  wk_start <- week_start(from)
  from <- vec_cast(from, new_date())
  if (!is_missing(to)) {
    to <- vec_cast(to, new_date())
  }
  if (is_missing(by)) {
    new_yearweek(seq_date(
      from = from, to = to, length.out = length.out,
      along.with = along.with, ...
    ), wk_start)
  } else {
    bad_by(by)
    by_week <- paste(by, "week")
    new_yearweek(seq_date(
      from = from, to = to, by = by_week, length.out = length.out,
      along.with = along.with, ...
    ), wk_start)
  }
}

#' @rdname year-week
#' @param year A vector of integers.
#' @inheritParams yearweek
#' @return `TRUE`/`FALSE` if the year has 53 ISO weeks.
#' @export
#' @examples
#' is_53weeks(2015:2016)
#' is_53weeks(1969)
#' is_53weeks(1969, week_start = 7)
is_53weeks <- function(year,
                       week_start = getOption("lubridate.week.start", 1)) {
  if (is_empty(year)) return(FALSE)

  if (!is_integerish(year) || any(year < 1)) {
    abort("Argument `year` must be positive integers.")
  }
  last_days <- make_date(year, 12, 31)
  last_weeks <- floor_date(last_days, "week", week_start = week_start)
  extract_week(last_weeks, week_start) == 53
}

#' @export
year.yearweek <- function(x) {
  wk <- extract_week(x, week_start(x))
  x <- as_date(x)
  mth <- month(x)
  year(x) - (mth == 1 & wk == 53) + (mth == 12 & wk == 1)
}

extract_week <- function(x, week_start) {
  date <- as_date(x) + (4 - wday(x, week_start = week_start))
  jan1 <- as.double(make_date(year(date), 1, 1))
  1 + (as.double(date) - jan1) %/% 7
}

convert_week_to_date <- function(year, week, week_start) {
  shift_num <- vec_init_along(year)
  for (i in seq_along(year)) {
    yr_wk_max <- (is_53weeks(1970:year[i], week_start) + 52)
    sign <- sign(year[i] - 1970)
    if (sign > 0) {
      nweeks <- head(yr_wk_max * sign, -1)
    } else {
      nweeks <- tail(yr_wk_max * sign, -1)
    }
    shift_num[i] <- sum(nweeks) + week[i] - 1
  }
  yearweek(0, week_start) + shift_num
}

#' @export
union.yearweek <- function(x, y, ...) {
  if (!inherits(y, "yearweek")) {
    stop("'y' must be of class 'yearweek'")
  }
  unique(vec_c(x, y))
}

#' @export
intersect.yearweek <- function(x, y, ...) {
  if (!inherits(y, "yearweek")) {
    stop("'y' must be of class 'yearweek'")
  }
  x[vec_in(x,y)]
}

#' @export
setdiff.yearweek <- function(x, y, ...) {
  if (!inherits(y, "yearweek")) {
    stop("'y' must be of class 'yearweek'")
  }
  c(x[!vec_in(x, y)], y[!vec_in(y, x)])
}
