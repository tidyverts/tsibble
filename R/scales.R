#' tsibble scales for ggplot2
#'
#' Defines ggplot2 scales for tsibble custom index: [yearweek], [yearmonth],
#' and [yearquarter].
#'
#' @param ... Arguments passed to [`ggplot2::scale_x_continuous()`].
#'
#' @return A ggproto object inheriting from `Scale`
#' @keywords internal
#'
#' @name tsibble-scales
NULL

scale_fun_pkg_check <- function() {
  pkg_not_available("ggplot2", "3.3.0")
  pkg_not_available("scales", "1.1.0")
}

scale_type.yearquarter <- function(x) c("yearquarter", "continuous")

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
      yearquarter(scales::breaks_pretty()(as_date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @export
scale_x_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_x_continuous(...),
    trans = yearquarter_trans())
}

#' @rdname tsibble-scales
#' @export
scale_y_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_y_continuous(...),
    trans = yearquarter_trans())
}

#' @rdname tsibble-scales
#' @export
scale_colour_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_colour_continuous(...),
    trans = yearquarter_trans())
}

#' @rdname tsibble-scales
#' @export
scale_color_yearquarter <- scale_colour_yearquarter

#' @rdname tsibble-scales
#' @export
scale_alpha_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_alpha_continuous(...),
    trans = yearquarter_trans())
}

#' @rdname tsibble-scales
#' @export
scale_fill_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_fill_continuous(...),
    trans = yearquarter_trans())
}

#' @rdname tsibble-scales
#' @export
scale_size_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_size_continuous(...),
    trans = yearquarter_trans())
}

scale_type.yearmonth <- function(x) c("yearmonth", "continuous")

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
      yearmonth(scales::breaks_pretty()(as_date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @export
scale_x_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_x_continuous(...),
    trans = yearmonth_trans())
}

#' @rdname tsibble-scales
#' @export
scale_y_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_y_continuous(...),
    trans = yearmonth_trans())
}

#' @rdname tsibble-scales
#' @export
scale_colour_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_colour_continuous(...),
    trans = yearmonth_trans())
}

#' @rdname tsibble-scales
#' @export
scale_color_yearmonth <- scale_colour_yearmonth

#' @keywords internal
#' @rdname tsibble-scales
#' @export
scale_alpha_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_alpha_continuous(...),
    trans = yearmonth_trans())
}

#' @rdname tsibble-scales
#' @export
scale_fill_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_fill_continuous(...),
    trans = yearmonth_trans())
}

#' @rdname tsibble-scales
#' @export
scale_size_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_size_continuous(...),
    trans = yearmonth_trans())
}

scale_type.yearweek <- function(x) c("yearweek", "continuous")

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
      yearweek(scales::breaks_pretty()(as_date(x)))
    }
  )
}

#' @rdname tsibble-scales
#' @export
scale_x_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_x_continuous(...),
    trans = yearweek_trans())
}

#' @rdname tsibble-scales
#' @export
scale_y_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_y_continuous(...),
    trans = yearweek_trans())
}

#' @rdname tsibble-scales
#' @export
scale_colour_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_colour_continuous(...),
    trans = yearweek_trans())
}

#' @rdname tsibble-scales
#' @export
scale_color_yearweek <- scale_colour_yearweek

#' @rdname tsibble-scales
#' @export
scale_alpha_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_alpha_continuous(...),
    trans = yearweek_trans())
}

#' @rdname tsibble-scales
#' @export
scale_fill_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_fill_continuous(...),
    trans = yearweek_trans())
}

#' @rdname tsibble-scales
#' @export
scale_size_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_size_continuous(...),
    trans = yearweek_trans())
}
