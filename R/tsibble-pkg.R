#' tsibble: temporal data frames and tools
#'
#' The **tsibble** package provides a data class of `tbl_ts` to manage temporal
#' data frames in a tidy and modern way. A tsibble consists of a time index, 
#' keys and other measured variables in a data-centric format, which is built on 
#' top of the tibble. 
#'
#' @inheritSection as_tsibble Index
#'
#' @inheritSection as_tsibble Key
#'
#' @name tsibble-package
#' @docType package
#' @useDynLib tsibble, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @import rlang
"_PACKAGE"

