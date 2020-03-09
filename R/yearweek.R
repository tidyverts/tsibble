#' Represent year-week (ISO) starting on Monday
#'
#' @description
#' \lifecycle{stable}
#'
#' Create or coerce using `yearweek()`.
#'
#' @param x Other object.
#'
#' @return year-week (`yearweek`) objects.
#'
#' @family index functions
#' @rdname year-week
#' @export
#' @examples
#' # coerce POSIXct/Dates to yearweek
#' x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 week")
#' yearweek(x)
#'
#' # parse characters
#' yearweek(c("2018 W01", "2018 Wk01", "2018 Week 1"))
#'
#' # creat an empty yearweek container
#' yearweek()
#'
#' # seq() and arithmetic
#' wk1 <- yearweek("2017 W50")
#' wk2 <- yearweek("2018 W12")
#' seq(from = wk1, to = wk2, by = 2)
#' wk1 + 0:9
#'
#' # display formats
#' format(c(wk1, wk2), format = "%V/%Y")
#'
#' # units since 1969-12-29
#' as.double(yearweek("1969 W41") + 0:24)
yearweek <- function(x) {
  UseMethod("yearweek")
}

#' @export
yearweek.default <- function(x) {
  dont_know(x, "yearweek")
}

#' @export
yearweek.NULL <- function(x) {
  new_yearweek()
}

#' @export
yearweek.numeric <- function(x) {
  new_yearweek(-3) + x
}

#' @export
yearweek.POSIXt <- function(x) {
  new_yearweek(floor_date(as_date(x), unit = "weeks", week_start = 1))
}

#' @export
yearweek.Date <- yearweek.POSIXt

#' @export
yearweek.character <- function(x) {
  key_words <- regmatches(x, gregexpr("[[:alpha:]]+", x))
  if (all(grepl("^(w|wk|week)$", key_words, ignore.case = TRUE))) {
    yr_week <- regmatches(x, gregexpr("[[:digit:]]+", x))
    digits_lgl <- map_lgl(yr_week, ~ !has_length(.x, 2))
    digits_len <- map_int(yr_week, ~ sum(nchar(.x)))
    if (any(digits_lgl) || any(digits_len < 5)) {
      abort("Character strings are not in a standard unambiguous format.")
    }
    yr_lgl <- map(yr_week, ~ grepl("[[:digit:]]{4}", .x))
    yr <- as.integer(map2_chr(yr_week, yr_lgl, ~ .x[.y]))
    week <- as.integer(map2_chr(yr_week, yr_lgl, ~ .x[!.y]))
    if (any(week > 53)) {
      abort("Weeks can't be greater than 53.")
    }
    check_53 <- !is_53weeks(yr) & (week > 52)
    if (any(check_53)) {
      abort(sprintf("Year %s can't be 53 weeks.", comma(yr[check_53])))
    }
    last_mon_last_year <- make_date(yr, 1, 5) - wday(make_date(yr, 1, 3)) - 7
    new_yearweek(last_mon_last_year + week * 7)
  } else {
    assertDate(x)
    yearweek(anydate(x))
  }
}

#' @export
yearweek.yearweek <- function(x) {
  x
}

new_yearweek <- function(x = double()) {
  new_vctr(x, class = "yearweek")
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

# diff.yearweek <- function(x, lag = 1, differences = 1, ...) {
#   out <- diff((as_date(x) - as_date("1969-12-29")) / 7,
#     lag = lag, differences = differences
#   )
#   structure(out, class = "difftime", units = "weeks")
# }

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_cast yearweek
#' @export
#' @export vec_cast.yearweek
vec_cast.yearweek <- function(x, to, ...) {
  UseMethod("vec_cast.yearweek")
}

#' @method vec_cast.Date yearweek
#' @export
vec_cast.Date.yearweek <- function(x, to, ...) {
  new_date(as.double(vec_data(x)))
}

#' @export
as.Date.yearweek <- function(x, ...) {
  new_date(as.double(vec_data(x)))
}

#' @method vec_cast.POSIXct yearweek
#' @export
vec_cast.POSIXct.yearweek <- function(x, to, ...) {
  as.POSIXct(new_date(x), ...)
}

#' @method vec_cast.double yearweek
#' @export
vec_cast.double.yearweek <- function(x, to, ...) {
  as.double((as_date(x) - as_date("1969-12-29")) / 7)
}

#' @export
as.POSIXlt.yearweek <- function(x, tz = "", ...) {
  as.POSIXlt(new_date(x), tz = tz, ...)
}

#' @method vec_cast.POSIXlt yearweek
#' @export
vec_cast.POSIXlt.yearweek <- function(x, to, ...) { # not working
  as.POSIXlt(new_date(x), ...)
}

#' @method vec_cast.yearweek yearweek
#' @export
vec_cast.yearweek.yearweek <- function(x, to, ...) {
  new_yearweek(x)
}

#' @method vec_cast.character yearweek
#' @export
vec_cast.character.yearweek <- function(x, to, ...) {
  format(x)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_ptype2 yearweek
#' @export
#' @export vec_ptype2.yearweek
vec_ptype2.yearweek <- function(x, y, ...) {
  UseMethod("vec_ptype2.yearweek", y)
}

#' @method vec_ptype2.yearweek POSIXt
#' @export
vec_ptype2.yearweek.POSIXt <- function(x, y, ...) {
  new_datetime()
}

#' @method vec_ptype2.POSIXt yearweek
#' @export
vec_ptype2.POSIXt.yearweek <- function(x, y, ...) {
  new_datetime()
}

#' @method vec_ptype2.yearweek Date
#' @export
vec_ptype2.yearweek.Date <- function(x, y, ...) {
  new_date()
}

#' @method vec_ptype2.yearweek yearweek
#' @export
vec_ptype2.yearweek.yearweek <- function(x, y, ...) {
  new_yearweek()
}

#' @method vec_ptype2.Date yearweek
#' @export
vec_ptype2.Date.yearweek <- function(x, y, ...) {
  new_date()
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method vec_arith yearweek
#' @export
#' @export vec_arith.yearweek
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
    new_yearweek(as_date(x) + y * 7)
  } else if (op == "-") {
    new_yearweek(as_date(x) - y * 7)
  } else {
    stop_incompatible_op(op, x, y)
  }
}

#' @method vec_arith.numeric yearweek
#' @export
vec_arith.numeric.yearweek <- function(op, x, y, ...) {
  if (op == "+") {
    yearweek(x * 7 + as_date(y))
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
  x <- as_date(x)
  mth <- month(x)
  wk <- strftime(x, "%V")
  shift_year <- period(1, units = "year")
  lgl1 <- !is.na(x) & mth == 1 & wk == "53"
  lgl2 <- !is.na(x) & mth == 12 & wk == "01"
  x[lgl1] <- x[lgl1] - shift_year
  x[lgl2] <- x[lgl2] + shift_year
  wk_sub <- map_chr(wk, ~ gsub("%V", ., x = format))
  wk_sub[is.na(wk_sub)] <- "-"
  format.Date(x, format = wk_sub)
}

#' @rdname vctrs-compat
#' @keywords internal
#' @method obj_print_data yearweek
#' @export
#' @export obj_print_data.yearweek
obj_print_data.yearweek <- function(x, ...) {
  if (length(x) == 0) return()
  print(format(x))
}

#' @export
vec_ptype_abbr.yearweek <- function(x, ...) {
  "week"
}

#' @export
seq.yearweek <- function(from, to, by, length.out = NULL, along.with = NULL,
                         ...) {
  bad_by(by)
  by_mth <- paste(by, "week")
  from <- vec_cast(from, new_date())
  if (!is_missing(to)) {
    to <- vec_cast(to, new_date())
  }
  new_yearweek(seq_date(
    from = from, to = to, by = by_mth, length.out = length.out,
    along.with = along.with, ...
  ))
}

#' @rdname year-week
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

#' @rdname year-week
#' @export
year.yearweek <- function(x) {
  x <- as_date(x)
  mth <- month(x)
  wk <- strftime(x, "%V")
  year(x) - (mth == 1 & wk == "53") + (mth == 12 & wk == "01")
}
