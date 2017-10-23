#' tsibble: temporal-context data frames and tools
#'
#' The **tsibble** package provides a data class of `tbl_ts` to manage temporal
#' data frames in a tidy and modern way. A tsibble consists of a time index, 
#' key(s) and other measured variables in a data-centric format, which is built on 
#' top of the tibble. 
#'
#' @section Index:
#' The time indices are no longer an attribute (for example, the `tsp` attribute 
#' in a `ts` object), but preserved as the essential component of the tsibble. A 
#' few index class, such as `Date`, `POSIXt`, and `difftime`, forms the basis of 
#' the tsibble, with new additions [yearmth], [yearqtr] and [year] representing 
#' year-month, year-quarter and year objects respectively.
#'
#' @section Key:
#' Key variable(s) together with the index uniquely identifies each record. And
#' key(s) also imposes the structure on a tsibble to describe other measured
#' variables:
#' * None: an implicit variable
#' * A single variable: an explicit variable. For example, `data(pedestrian)`
#' uses the `Sensor` column as the key.
#' * Nested variables: a nesting of one variable under another. For example, 
#' `data(tourism)` contains two geographical locations: `Region` and `State`.
#' `Region` is the lower level than `State` in Australia; in other words, `Region`
#' is nested into `State`, which naturally forms a hierarchy. A vertical bars (|)
#' is used to describe this nesting relationship, and thus `Region` | `State`. 
#' In theory, nesting can involve infinite levels, so is `tsibble`.
#' * Crossed variables: a crossing of one variable with another. For example,
#' the geographical locations are crossed with the purpose of visiting (`Purpose`)
#' in the `data(tourism)`. A comma (,) is used to indicate this crossing
#' relationship. Nested and crossed variables can be combined, such as `data(tourism)`.
#'
#' These key variables are descriptors, not values.
#'
#' @section Print options:
#' The tsibble package fully utilises the `print` method from the tibble. Please
#' refer to [tibble::tibble-package] to change display options.
#'
#' @rdname tsibble-package
#' @name tsibble-package
#' @docType package
#' @useDynLib tsibble, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import rlang
"_PACKAGE"

