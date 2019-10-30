#' tsibble scales for ggplot2
#'
#' Defines ggplot2 scales for tsibble custom index: [yearweek], [yearmonth], and [yearquarter].
#'
#' @param ... Further arguments to be passed on to [`ggplot2::scale_x_date()`]
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @name tsibble-scales
NULL

scale_type.yearquarter <- function(x) c("yearquarter", "date", "continuous")

yearquarter_trans <- function() {
  scales::trans_new(
    "yearquarter",
    transform = function(x) {
      scales::date_trans()$transform(as_date(x))
    },
    inverse = function(x) {
      yearquarter(scales::date_trans()$inverse(x))
    },
    breaks = function(x) {
      yearquarter(scales::pretty_breaks()(as_date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @inheritParams ggplot2::scale_x_datetime
#' @export
scale_x_yearquarter <- function(...) {
  pkg_not_available("ggplot2")
  pkg_not_available("scales")
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_x_date(...),
    trans = yearquarter_trans())
}

scale_type.yearmonth <- function(x) c("yearmonth", "date", "continuous")

yearmonth_trans <- function() {
  scales::trans_new(
    "yearmonth",
    transform = function(x) {
      scales::date_trans()$transform(as_date(x))
    },
    inverse = function(x) {
      yearmonth(scales::date_trans()$inverse(x))
    },
    breaks = function(x) {
      yearmonth(scales::pretty_breaks()(as_date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @inheritParams ggplot2::scale_x_datetime
#' @export
scale_x_yearmonth <- function(...) {
  pkg_not_available("ggplot2")
  pkg_not_available("scales")
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_x_date(...),
    trans = yearmonth_trans())
}

scale_type.yearweek <- function(x) c("yearweek", "date", "continuous")

yearweek_trans <- function() {
  scales::trans_new(
    "yearweek",
    transform = function(x) {
      scales::date_trans()$transform(as_date(x))
    },
    inverse = function(x) {
      yearweek(scales::date_trans()$inverse(x))
    },
    breaks = function(x) {
      yearweek(scales::pretty_breaks()(as_date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @inheritParams ggplot2::scale_x_datetime
#' @export
scale_x_yearweek <- function(...) {
  pkg_not_available("ggplot2")
  pkg_not_available("scales")
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_x_date(...),
    trans = yearweek_trans())
}
