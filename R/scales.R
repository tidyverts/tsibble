# nocov start

#' tsibble scales for ggplot2
#'
#' Defines ggplot2 scales for tsibble custom index: [yearweek], [yearmonth],
#' and [yearquarter].
#'
#' @param ... Arguments passed to [`ggplot2::scale_x_date()`].
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @name tsibble-scales
NULL

scale_fun_pkg_check <- function() {
  pkg_not_available("ggplot2", "3.3.0")
  pkg_not_available("scales", "1.1.0")
}

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
    breaks = yearquarter_breaks()
  )
}

yearquarter_get_breaks <- function(self, limits = self$get_limits()) {
  breaks <- ggproto_parent(ScaleContinuous, self)$get_breaks(limits)

  # (non-)redundant censoring because of non-invertibility of transforms
  breaks <- scales::censor(breaks, limits, only.finite = FALSE)

  breaks
}

yearquarter_breaks <- function(n = 5) {
  force(n)
  function(x) {
    yearquarter(scales::breaks_pretty(n)(as_date(x)))
  }
}

fullseq.yearquarter <- function(range, size, ...) {
  scales::fullseq(as_date(range), size = size, ...)
}

#' @rdname tsibble-scales
#' @export
scale_x_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_x_date(...),
    trans = yearquarter_trans(),
    get_breaks = yearquarter_get_breaks
    )
}

#' @rdname tsibble-scales
#' @export
scale_y_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_y_date(...),
    trans = yearquarter_trans(),
    get_breaks = yearquarter_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_colour_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_colour_date(...),
    trans = yearquarter_trans(),
    get_breaks = yearquarter_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_color_yearquarter <- scale_colour_yearquarter

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_alpha_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_alpha_date(...),
    trans = yearquarter_trans(),
    get_breaks = yearquarter_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_fill_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_fill_date(...),
    trans = yearquarter_trans(),
    get_breaks = yearquarter_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_size_yearquarter <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_size_date(...),
    trans = yearquarter_trans(),
    get_breaks = yearquarter_get_breaks)
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
    breaks = yearmonth_breaks()
  )
}

yearmonth_get_breaks <- yearquarter_get_breaks

yearmonth_breaks <- function(n = 5) {
  force(n)
  function(x) {
    yearmonth(scales::breaks_pretty(n)(as_date(x)))
  }
}

fullseq.yearmonth <- function(range, size, ...) {
  scales::fullseq(as_date(range), size = size, ...)
}

#' @rdname tsibble-scales
#' @export
scale_x_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_x_date(...),
    trans = yearmonth_trans(),
    get_breaks = yearmonth_get_breaks)
}

#' @rdname tsibble-scales
#' @export
scale_y_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_y_date(...),
    trans = yearmonth_trans(),
    get_breaks = yearmonth_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_colour_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_colour_date(...),
    trans = yearmonth_trans(),
    get_breaks = yearmonth_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_color_yearmonth <- scale_colour_yearmonth

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_alpha_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_alpha_date(...),
    trans = yearmonth_trans(),
    get_breaks = yearmonth_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_fill_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_fill_date(...),
    trans = yearmonth_trans(),
    get_breaks = yearmonth_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_size_yearmonth <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_size_date(...),
    trans = yearmonth_trans(),
    get_breaks = yearmonth_get_breaks)
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
    breaks = yearweek_breaks()
  )
}

yearweek_get_breaks <- yearquarter_get_breaks

yearweek_breaks <- function(n = 5) {
  force(n)
  function(x) {
    yearweek(scales::breaks_pretty(n)(as_date(x)))
  }
}

fullseq.yearweek <- function(range, size, ...) {
  scales::fullseq(as_date(range), size = size, ...)
}

#' @rdname tsibble-scales
#' @export
scale_x_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_x_date(...),
    trans = yearweek_trans(),
    get_breaks = yearweek_get_breaks)
}

#' @rdname tsibble-scales
#' @export
scale_y_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_y_date(...),
    trans = yearweek_trans(),
    get_breaks = yearweek_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_colour_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_colour_date(...),
    trans = yearweek_trans(),
    get_breaks = yearweek_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_color_yearweek <- scale_colour_yearweek

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_alpha_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_alpha_date(...),
    trans = yearweek_trans(),
    get_breaks = yearweek_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_fill_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_fill_date(...),
    trans = yearweek_trans(),
    get_breaks = yearweek_get_breaks)
}

#' @rdname tsibble-scales
#' @export
#' @usage NULL
scale_size_yearweek <- function(...) {
  scale_fun_pkg_check()
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_size_date(...),
    trans = yearweek_trans(),
    get_breaks = yearweek_get_breaks)
}

# nocov end
