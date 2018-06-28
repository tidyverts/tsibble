#' tsibble: tidy temporal data frames and tools
#'
#' The **tsibble** package provides a data class of `tbl_ts` to store and manage
#' temporal data frames in a "tidy" form. A tsibble consists of a time index, 
#' key(s) and other measured variables in a data-centric format, which is built on 
#' top of the tibble. 
#'
#' @section Index:
#' The time indices are no longer an attribute (for example, the `tsp` attribute 
#' in a `ts` object), but preserved as the essential component of the tsibble. A 
#' few index classes, such as `Date`, `POSIXct`, and `difftime`, forms the basis of 
#' the tsibble, with new additions [yearweek], [yearmonth], and [yearquarter] 
#' representing year-week, year-month, and year-quarter respectively. `zoo::yearmth` 
#' and `zoo::yearqtr` are also supported. For a `tbl_ts` of regular interval,
#' a choice of index representation has to be made. For example, a monthly data 
#' should correspond to time index created by [yearmonth] or `zoo::yearmth`, 
#' instead of `Date` or `POSIXct`. Because months in a year ensures the regularity,
#' 12 months every year. However, if using `Date`, a month contains days ranging
#' from 28 to 31 days, which results in irregular time space. This is also applicable
#' to year-week and year-quarter.
#'
#' Since the **tibble** that underlies the **tsibble** only accepts a 1d atomic 
#' vector or a list, a `tbl_ts` doesn't accept `POSIXlt` and `timeDate` columns.
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
#' These key variables describe the data structure.
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
#' @aliases NULL tsibble-package
#' @useDynLib tsibble, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom purrr map map_dbl map_int map_chr map_lgl
#' @importFrom purrr map2 map2_dbl map2_int map2_chr map2_lgl
#' @importFrom purrr pmap pmap_dbl pmap_int pmap_chr pmap_lgl
#' @import rlang
#' @examples
#' # create a tsibble w/o a key ----
#' tsbl1 <- tsibble(
#'   date = seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1),
#'   value = rnorm(10),
#'   key = id(), index = date
#' )
#' tsbl1
#'
#' # create a tsibble with one key ----
#' tsbl2 <- tsibble(
#'   qtr = rep(yearquarter(seq(2010, 2012.25, by = 1 / 4)), 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30),
#'   key = id(group), index = qtr
#' )
#' tsbl2
"_PACKAGE"

