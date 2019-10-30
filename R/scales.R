#' tsibble scales for ggplot2
#'
#' Defines ggplot2 scales for tsibble time structures.
#'
#' @param ... Further arguments to be passed on to scale_x_date()
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
      scales::date_trans()$transform(as.Date(x))
    },
    inverse = function(x) {
      yearquarter(scales::date_trans()$inverse(x))
    },
    breaks = function(x) {
      yearquarter(scales::pretty_breaks()(as.Date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @inheritParams ggplot2::scale_x_datetime
#' @export
scale_x_yearquarter <- function(...) {
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_x_date(...),
    trans = yearquarter_trans())
}

scale_type.yearmonth <- function(x) c("yearmonth", "date", "continuous")

yearmonth_trans <- function() {
  scales::trans_new(
    "yearmonth",
    transform = function(x) {
      scales::date_trans()$transform(as.Date(x))
    },
    inverse = function(x) {
      yearmonth(scales::date_trans()$inverse(x))
    },
    breaks = function(x) {
      yearmonth(scales::pretty_breaks()(as.Date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @inheritParams ggplot2::scale_x_datetime
#' @export
scale_x_yearmonth <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort("Package `ggplot2` required.\nPlease install and try again.")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    abort("Package `scales` required.\nPlease install and try again.")
  }

  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_x_date(...),
    trans = yearmonth_trans())
}

scale_type.yearweek <- function(x) c("yearweek", "date", "continuous")

yearweek_trans <- function() {
  scales::trans_new(
    "yearweek",
    transform = function(x) {
      scales::date_trans()$transform(as.Date(x))
    },
    inverse = function(x) {
      yearweek(scales::date_trans()$inverse(x))
    },
    breaks = function(x) {
      yearweek(scales::pretty_breaks()(as.Date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @inheritParams ggplot2::scale_x_datetime
#' @export
scale_x_yearweek <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort("Package `ggplot2` required.\nPlease install and try again.")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    abort("Package `scales` required.\nPlease install and try again.")
  }

  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_x_date(...),
    trans = yearweek_trans())
}
