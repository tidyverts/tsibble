#' tsibble: temporal data frames and tools
#'
#' The **tsibble** package provides a data class of `tbl_ts` to manage temporal
#' data frames in a tidy and modern way. A tsibble consists of a time index, 
#' keys and other measured variables in a data-centric format, which is built on 
#' top of the tibble. 
#'
#' @section Index:
#' The time indices are no longer an attribute (for example, the `tsp` attribute 
#' in a `ts` object), but preserved as the essential component of the tsibble. A 
#' few index class, such as `Date`, `POSIXt`, and `difftime`, forms the basis of 
#' the tsibble, with new additions [yearmth], [yearqtr] and [year] representing 
#' year-month, year-quarter and year objects respectively.
#'
#' @inheritSection as_tsibble Key(s)
#'
#' @name tsibble-package
#' @docType package
#' @useDynLib tsibble, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import rlang
"_PACKAGE"

