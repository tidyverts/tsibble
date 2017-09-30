#' tsibble: temporal data frames and tools
#'
#' The **tsibble** package provides a data class of `tbl_ts` to manage temporal
#' data frames in a tidy and modern way. A tsibble consists of a time index, 
#' keys and other measured variables in a long data format, built on top of the 
#' [tibble]. 
#'
#' @section Index:
#' The time indices are no longer an attribute (for example, the `tsp` attribute 
#' in a `ts` object), but the essential component of the tsibble. A few index 
#' class, such as `Date`, `POSIXt`, and `difftime`, are undoubtedly supported by 
#' the tsibble, with two new additions `yearmth` and `yearqtr` representing 
#' year-month and year-quarter index.
#'
#' @section Key(s):
#' A sequence of time indices have to be unique for each key or each combination
#' of key variables.
#'
#' @name tsibble-package
#' @docType package
#' @useDynLib tsibble, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import rlang
"_PACKAGE"

