#' tsibble: tidy temporal data frames and tools
#'
#' The **tsibble** package provides a data class of `tbl_ts` to store and manage
#' temporal data frames in a tidy manner. A tsibble consists of a time index, 
#' key(s) and other measured variables in a data-centric format, which is built on 
#' top of the tibble. 
#'
#' @section Index:
#' The time indices are no longer an attribute (for example, the `tsp` attribute 
#' in a `ts` object), but preserved as the essential component of the tsibble. A 
#' few index classes, such as `Date`, `POSIXct`, and `difftime`, forms the basis of 
#' the tsibble, with new additions [yearmonth] and [yearquarter] representing 
#' year-month and year-quarter respectively. `zoo::yearmth` and `zoo::yearqtr`
#' are also supported. It is extensible to work with custom index, for example
#' trading days and weekly data.
#'
#' @section Key:
#' Key variable(s) together with the index uniquely identifies each record. And
#' key(s) also imposes the structure on a tsibble, which can be created via the
#' [id] function as identifiers:
#' * None: an implicit variable `id()` resulting a univariate time series.
#' * A single variable: an explicit variable. For example, `data(pedestrian)`
#' uses the `id(Sensor)` column as the key.
#' * Nested variables: a nesting of one variable under another. For example, 
#' `data(tourism)` contains two geographical locations: `Region` and `State`.
#' `Region` is the lower level than `State` in Australia; in other words, `Region`
#' is nested into `State`, which naturally forms a hierarchy. A vertical bar (`|`)
#' is used to describe this nesting relationship, and thus `Region` | `State`. 
#' In theory, nesting can involve infinite levels, so is `tsibble`.
#' * Crossed variables: a crossing of one variable with another. For example,
#' the geographical locations are crossed with the purpose of visiting (`Purpose`)
#' in the `data(tourism)`. A comma (`,`) is used to indicate this crossing
#' relationship. Nested and crossed variables can be combined, such as 
#' `data(tourism)` using `id(Region | State, Purpose)`.
#'
#' These key variables are descriptors, not values.
#'
#' @section Interval:
#' The [interval] function returns the interval associated with the tsibble.
#' * Regular: the value and its time unit including "second", "minute", "hour", 
#' "day", "week", "month", "quarter", "year". An unrecognisable time interval is
#' labelled as "unit".
#' * Irregular: `as_tsibble(regular = FALSE)` gives the irregular tsibble. It is
#' marked with `!`.
#' * Unknown: if there is only one entry for each key variable, the interval
#' cannot be determined (`?`).
#'
#' @section Print options:
#' The tsibble package fully utilises the `print` method from the tibble. Please
#' refer to [tibble::tibble-package] to change display options.
#'
#' @rdname tsibble-package
#' @name tsibble-package
#' @docType package
#' @aliases NULL
#' @useDynLib tsibble, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import rlang
"_PACKAGE"

