#' tsibble: tidy temporal data frames and tools
#'
#' \if{html}{\figure{logo.png}{options: style='text-align: right;'}}
#' The **tsibble** package provides a data class of `tbl_ts` to represent tidy
#' temporal data. A tsibble consists of a time index, key, and other measured
#' variables in a data-centric format, which is built on top of the tibble.
#'
#' @section Index:
#' An extensive range of indices are supported by tsibble:
#' * native time classes in R (such as `Date`, `POSIXct`, and `difftime`)
#' * tsibble's new additions (such as [yearweek], [yearmonth], and [yearquarter]).
#' * other commonly-used classes: `ordered`, `hms::hms`, `lubridate::period`,
#' and `nanotime::nanotime`.
#'
#' For a `tbl_ts` of regular interval, a choice of index representation has to
#' be made. For example, a monthly data should correspond to time index created
#' by [yearmonth], instead of `Date` or `POSIXct`. Because months in a year
#' ensures the regularity, 12 months every year. However, if using `Date`, a
#' month containing days ranges from 28 to 31 days, which results in irregular
#' time space. This is also applicable to year-week and year-quarter.
#'
#' Tsibble supports arbitrary index classes, as long as they can be ordered from
#' past to future. To support a custom class, you need to define [index_valid()]
#' for the class and calculate the interval through [interval_pull()].
#'
#' @section Key:
#' Key variable(s) together with the index uniquely identifies each record:
#' * Empty: an implicit variable. `NULL` resulting in a univariate time series.
#' * A single variable: For example, `data(pedestrian)` uses `Sensor` as the key.
#' * Multiple variables: For example, Declare `key = c(Region, State, Purpose)`
#' for `data(tourism)`.
#' Key can be created in conjunction with tidy selectors like `starts_with()`.
#'
#' @section Interval:
#' The [interval] function returns the interval associated with the tsibble.
#' * Regular: the value and its time unit including "nanosecond", "microsecond",
#' "millisecond", "second", "minute", "hour", "day", "week", "month", "quarter",
#' "year". An unrecognisable time interval is labelled as "unit".
#' * Irregular: `as_tsibble(regular = FALSE)` gives the irregular tsibble. It is
#' marked with `!`.
#' * Unknown: Not determined (`?`), if it's an empty tsibble, or one entry for
#' each key variable.
#'
#' An interval is obtained based on the corresponding index representation:
#' * integerish numerics between 1582 and 2499: "year" (`Y`). Note the year of
#' 1582 saw the beginning of the Gregorian Calendar switch.
#' * `yearquarter`: "quarter" (`Q`)
#' * `yearmonth`: "month" (`M`)
#' * `yearweek`: "week" (`W`)
#' * `Date`: "day" (`D`)
#' * `difftime`: "week" (`W`), "day" (D), "hour" (`h`), "minute" (`m`), "second" (`s`)
#' * `POSIXt`/`hms`: "hour" (`h`), "minute" (`m`), "second" (`s`), "millisecond" (`us`),
#' "microsecond" (`ms`)
#' * `period`: "year" (`Y`), "month" (`M`), "day" (`D`), "hour" (`h`),
#' "minute" (`m`), "second" (`s`), "millisecond" (`us`), "microsecond" (`ms`)
#' * `nanotime`: "nanosecond" (`ns`)
#' * other numerics &`ordered` (ordered factor): "unit"
#' When the interval cannot be obtained due to the mismatched index format, an
#' error is issued.
#'
#' The interval is invariant to subsetting, such as `filter()`, `slice()`, and `[.tbl_ts`.
#' However, if the result is an empty tsibble, the interval is always unknown.
#' When joining a tsibble with other data sources and aggregating to different
#' time scales, the interval gets re-calculated.
#'
#' @section Time zone:
#' Time zone corresponding to index will be displayed if index is `POSIXct`.
#' `?` means that the obtained time zone is a zero-length character "".
#'
#' @section Print options:
#' The tsibble package fully utilises the `print` method from the tibble. Please
#' refer to [tibble::tibble-package] to change display options.
#'
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
#'   key = group
#' )
"_PACKAGE"

## This function is never called
## Exists to suppress this NOTE:
## "Namespaces in Imports field not imported from:"
## https://github.com/opencpu/opencpu/blob/10469ee3ddde0d0dca85bd96d2873869d1a64cd6/R/utils.R#L156-L165
stub <- function() {
  ## I have to use curl directly somewhere, if I import it.
  ## I have to import it if I want to state a minimum version.
  if (FALSE) lifecycle::deprecate_soft()
}

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom utils head tail
#' @importFrom stats frequency start end time ts as.ts tsp<-
#' @importFrom anytime assertTime assertDate utcdate utctime anydate
#' @importFrom anytime addFormats removeFormats
#' @importFrom lubridate make_date wday floor_date year month period as.period
#' @importFrom lubridate as.duration
#' @importFrom lubridate quarter is.POSIXct rollback force_tz tz is.POSIXt is.Date
#' @importFrom lubridate decimal_date date_decimal round_date
#' @importFrom lubridate seconds_to_period month<- weeks days as_date
#' @importFrom dplyr arrange filter select mutate transmute summarise rename
#' @importFrom dplyr group_by ungroup group_data grouped_df group_vars
#' @importFrom dplyr group_rows groups new_grouped_df is_grouped_df
#' @importFrom dplyr left_join right_join full_join inner_join anti_join
#' @importFrom dplyr bind_rows bind_cols combine with_order pull first reframe
#' @importFrom dplyr dplyr_row_slice dplyr_col_modify dplyr_reconstruct
#' @importFrom tibble new_tibble trunc_mat
#' @import rlang tidyselect vctrs ellipsis
## usethis namespace: end
NULL
