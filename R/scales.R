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

#' @rdname tsibble-scales
#' @inheritParams ggplot2::scale_x_datetime
#' @export
scale_x_yearquarter <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    abort("Package `ggplot2` required.\nPlease install and try again.")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    abort("Package `scales` required.\nPlease install and try again.")
  }

  yq_trans <- scales::trans_new(
    "yearquarter",
    transform = function(x){
      # Transform to date, then use scales date transformation method
      scales::date_trans()$transform(as.Date(x))
    }, inverse = function(x){
      # Invert to date, then convert to yearquarter
      yearquarter(scales::date_trans()$inverse(x))
    },
    breaks = function(x){
      # Use date breaks method, then convert to yearquarter
      yearquarter(scales::pretty_breaks()(as.Date(x)))
    }
  )
  ggplot2::ggproto("ScaleContinuousYearquarter", ggplot2::scale_x_date(...),
                   trans = yq_trans)
}

scale_type.yearmonth <- function(x) c("yearmonth", "date", "continuous")

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

  ym_trans <- scales::trans_new(
    "yearmonth",
    transform = function(x){
      # Transform to date, then use scales date transformation method
      scales::date_trans()$transform(as.Date(x))
    }, inverse = function(x){
      # Invert to date, then convert to yearmonth
      yearmonth(scales::date_trans()$inverse(x))
    },
    breaks = function(x){
      # Use date breaks method, then convert to yearmonth
      yearmonth(scales::pretty_breaks()(as.Date(x)))
    }
  )
  ggplot2::ggproto("ScaleContinuousYearmonth", ggplot2::scale_x_date(...),
                   trans = ym_trans)
}

scale_type.yearweek <- function(x) c("yearweek", "date", "continuous")

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

  yw_trans <- scales::trans_new(
    "yearweek",
    transform = function(x){
      # Transform to date, then use scales date transformation method
      scales::date_trans()$transform(as.Date(x))
    }, inverse = function(x){
      # Invert to date, then convert to yearweek
      yearweek(scales::date_trans()$inverse(x))
    },
    breaks = function(x){
      # Use date breaks method, then convert to yearweek
      yearweek(scales::pretty_breaks()(as.Date(x)))
    }
  )
  ggplot2::ggproto("ScaleContinuousYearweek", ggplot2::scale_x_date(...),
                   trans = yw_trans)

}
