#' tsibble: tidy temporal data frames and tools
#'
#' \if{html}{\figure{logo.png}{options: align='right'}}
#' The **tsibble** package provides a data class of `tbl_ts` to represent tidy 
#' temporal data. A tsibble consists of a time index, key, and other measured 
#' variables in a data-centric format, which is built on top of the tibble. 
#'
#' @section Index:
#' The time indices are preserved as the essential data component of the tsibble,
#' instead of implicit attribute (for example, the `tsp` attribute in a `ts` object). A 
#' few index classes, such as `Date`, `POSIXct`, and `difftime`, forms the basis of 
#' the tsibble, with new additions [yearweek], [yearmonth], and [yearquarter] 
#' representing year-week, year-month, and year-quarter respectively. Any arbitrary
#' index class are also supported, including `zoo::yearmon`, `zoo::yearqtr`, and
#' `nanotime`. 
#' For a `tbl_ts` of regular interval,
#' a choice of index representation has to be made. For example, a monthly data 
#' should correspond to time index created by [yearmonth] or `zoo::yearmon`, 
#' instead of `Date` or `POSIXct`. Because months in a year ensures the regularity,
#' 12 months every year. However, if using `Date`, a month contains days ranging
#' from 28 to 31 days, which results in irregular time space. This is also applicable
#' to year-week and year-quarter.
#'
#' Since the **tibble** that underlies the **tsibble** only accepts a 1d atomic 
#' vector or a list, a `tbl_ts` doesn't accept `POSIXlt` and `timeDate` columns.
#'
#' @section Key:
#' Key variable(s) together with the index uniquely identifies each record, which 
#' can be created via the [id] function as identifiers:
#' * Empty: an implicit variable `id()` resulting in a univariate time series.
#' * One or more variables: explicit variables. For example, `data(pedestrian)`
#' and `data(tourism)` use the `id(Sensor)` & `id(Region, State, Purpose)` as 
#' the key respectively.
#'
#' @section Interval:
#' The [interval] function returns the interval associated with the tsibble.
#' * Regular: the value and its time unit including "nanosecond", "microsecond",
#' "millisecond", "second", "minute", "hour", "day", "week", "month", "quarter", 
#' "year". An unrecognisable time interval is labelled as "unit".
#' * Irregular: `as_tsibble(regular = FALSE)` gives the irregular tsibble. It is
#' marked with `!`.
#' * Unknown: if there is only one entry for each key variable, the interval
#' cannot be determined (`?`).
#'
#' An interval is obtained based on the corresponding index representation:
#' * integer/numeric: either "unit" or "year"
#' * `yearquarter`/`yearqtr`: "quarter"
#' * `yearmonth`/`yearmon`: "month"
#' * `yearweek`: "week"
#' * `Date`: "day"
#' * `POSIXct`: "hour", "minute", "second", "millisecond", "microsecond"
#' * `nanotime`: "nanosecond"
#'
#' @section Time zone:
#' Time zone corresponding to index will be displayed if index is `POSIXct`.
#' `?` means that the obtained time zone is a zero-length character "".
#'
#' @section Print options:
#' The tsibble package fully utilises the `print` method from the tibble. Please
#' refer to [tibble::tibble-package] to change display options.
#'
#' @aliases NULL tsibble-package
#' @useDynLib tsibble, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom utils head tail
#' @importFrom purrr map map_dbl map_int map_chr map_lgl
#' @importFrom purrr map2 map2_dbl map2_int map2_chr map2_lgl
#' @importFrom purrr pmap pmap_dbl pmap_int pmap_chr pmap_lgl
#' @import rlang
#' @examples
#' # create a tsibble w/o a key ----
#' tsibble(
#'   date = as.Date("2017-01-01") + 0:9,
#'   value = rnorm(10)
#' )
#'
#' # create a tsibble with one key ----
#' tsibble(
#'   qtr = rep(yearquarter("2010-01") + 0:9, 3),
#'   group = rep(c("x", "y", "z"), each = 10),
#'   value = rnorm(30),
#'   key = id(group)
#' )
"_PACKAGE"

