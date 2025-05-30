#' @export
summary.yearquarter <- function(object, digits = 12L, ...) {
  x <- summary.default(unclass(object), digits = digits, ...)
  if (m <- match("NA's", names(x), 0L)) {
    NAs <- as.integer(x[m])
    x <- x[-m]
    attr(x, "NAs") <- NAs
  }
  output <- yearquarter(.Date(unclass(x)))
  attributes(output) <- attributes(x)
  class(output) <- c("summarycal", class(object))
  output
}

#' @export
summary.yearmonth <- function(object, digits = 12L, ...) {
  x <- summary.default(unclass(object), digits = digits, ...)
  if (m <- match("NA's", names(x), 0L)) {
    NAs <- as.integer(x[m])
    x <- x[-m]
    attr(x, "NAs") <- NAs
  }
  output <- yearmonth(.Date(unclass(x)))
  attributes(output) <- attributes(x)
  class(output) <- c("summarycal", class(object))
  output
}

#' @export
format.summarycal <- function(x, ...) {
  xx <- x
  class(xx) <- class(x)[-1]
  xx <- c(
    format(xx),
    `NA's` = if (length(a <- attr(x, "NAs"))) as.character(a)
  )
  xx
}
