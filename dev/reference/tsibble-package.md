# tsibble: tidy temporal data frames and tools

The **tsibble** package provides a data class of `tbl_ts` to represent
tidy temporal data. A tsibble consists of a time index, key, and other
measured variables in a data-centric format, which is built on top of
the tibble.

## Index

An extensive range of indices are supported by tsibble:

- native time classes in R (such as `Date`, `POSIXct`, and `difftime`)

- tsibble's new additions (such as
  [yearweek](https://tsibble.tidyverts.org/dev/reference/year-week.md),
  [yearmonth](https://tsibble.tidyverts.org/dev/reference/year-month.md),
  and
  [yearquarter](https://tsibble.tidyverts.org/dev/reference/year-quarter.md)).

- other commonly-used classes: `ordered`,
  [`hms::hms`](https://hms.tidyverse.org/reference/hms.html),
  [`lubridate::period`](https://lubridate.tidyverse.org/reference/period.html),
  and
  [`nanotime::nanotime`](https://eddelbuettel.github.io/nanotime/man/nanotime.html).

For a `tbl_ts` of regular interval, a choice of index representation has
to be made. For example, a monthly data should correspond to time index
created by
[yearmonth](https://tsibble.tidyverts.org/dev/reference/year-month.md),
instead of `Date` or `POSIXct`. Because months in a year ensures the
regularity, 12 months every year. However, if using `Date`, a month
containing days ranges from 28 to 31 days, which results in irregular
time space. This is also applicable to year-week and year-quarter.

Tsibble supports arbitrary index classes, as long as they can be ordered
from past to future. To support a custom class, you need to define
[`index_valid()`](https://tsibble.tidyverts.org/dev/reference/index_valid.md)
for the class and calculate the interval through
[`interval_pull()`](https://tsibble.tidyverts.org/dev/reference/interval-pull.md).

## Key

Key variable(s) together with the index uniquely identifies each record:

- Empty: an implicit variable. `NULL` resulting in a univariate time
  series.

- A single variable: For example, `data(pedestrian)` uses `Sensor` as
  the key.

- Multiple variables: For example, Declare
  `key = c(Region, State, Purpose)` for `data(tourism)`. Key can be
  created in conjunction with tidy selectors like
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html).

## Interval

The [interval](https://tsibble.tidyverts.org/dev/reference/regular.md)
function returns the interval associated with the tsibble.

- Regular: the value and its time unit including "nanosecond",
  "microsecond", "millisecond", "second", "minute", "hour", "day",
  "week", "month", "quarter", "year". An unrecognisable time interval is
  labelled as "unit".

- Irregular: `as_tsibble(regular = FALSE)` gives the irregular tsibble.
  It is marked with `!`.

- Unknown: Not determined (`?`), if it's an empty tsibble, or one entry
  for each key variable.

An interval is obtained based on the corresponding index representation:

- integerish numerics between 1582 and 2499: "year" (`Y`). Note the year
  of 1582 saw the beginning of the Gregorian Calendar switch.

- `yearquarter`: "quarter" (`Q`)

- `yearmonth`: "month" (`M`)

- `yearweek`: "week" (`W`)

- `Date`: "day" (`D`)

- `difftime`: "week" (`W`), "day" (D), "hour" (`h`), "minute" (`m`),
  "second" (`s`)

- `POSIXt`/`hms`: "hour" (`h`), "minute" (`m`), "second" (`s`),
  "millisecond" (`us`), "microsecond" (`ms`)

- `period`: "year" (`Y`), "month" (`M`), "day" (`D`), "hour" (`h`),
  "minute" (`m`), "second" (`s`), "millisecond" (`us`), "microsecond"
  (`ms`)

- `nanotime`: "nanosecond" (`ns`)

- other numerics &`ordered` (ordered factor): "unit" When the interval
  cannot be obtained due to the mismatched index format, an error is
  issued.

The interval is invariant to subsetting, such as
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`slice()`](https://dplyr.tidyverse.org/reference/slice.html), and
`[.tbl_ts`. However, if the result is an empty tsibble, the interval is
always unknown. When joining a tsibble with other data sources and
aggregating to different time scales, the interval gets re-calculated.

## Time zone

Time zone corresponding to index will be displayed if index is
`POSIXct`. `?` means that the obtained time zone is a zero-length
character "".

## Print options

The tsibble package fully utilises the `print` method from the tibble.
Please refer to
[tibble::tibble-package](https://tibble.tidyverse.org/reference/tibble-package.html)
to change display options.

## See also

Useful links:

- <https://tsibble.tidyverts.org>

- Report bugs at <https://github.com/tidyverts/tsibble/issues>

## Author

**Maintainer**: Mitchell O'Hara-Wild <mail@mitchelloharawild.com>
([ORCID](https://orcid.org/0000-0001-6729-7695))

Authors:

- Earo Wang ([ORCID](https://orcid.org/0000-0001-6448-5260))

- Di Cook ([ORCID](https://orcid.org/0000-0002-3813-7155)) \[thesis
  advisor\]

- Rob Hyndman ([ORCID](https://orcid.org/0000-0002-2140-5352)) \[thesis
  advisor\]

Other contributors:

- Tyler Smith \[contributor\]

- Wil Davis <william.davis@worthingtonindustries.com> \[contributor\]

## Examples

``` r
# create a tsibble w/o a key ----
tsibble(
  date = as.Date("2017-01-01") + 0:9,
  value = rnorm(10)
)
#> Using `date` as index variable.
#> # A tsibble: 10 x 2 [1D]
#>    date        value
#>    <date>      <dbl>
#>  1 2017-01-01  0.451
#>  2 2017-01-02  0.497
#>  3 2017-01-03  0.779
#>  4 2017-01-04  1.51 
#>  5 2017-01-05 -1.82 
#>  6 2017-01-06  0.788
#>  7 2017-01-07 -0.973
#>  8 2017-01-08  0.893
#>  9 2017-01-09  0.763
#> 10 2017-01-10  0.651

# create a tsibble with one key ----
tsibble(
  qtr = rep(yearquarter("2010-01") + 0:9, 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30),
  key = group
)
#> Using `qtr` as index variable.
#> # A tsibble: 30 x 3 [1Q]
#> # Key:       group [3]
#>        qtr group   value
#>      <qtr> <chr>   <dbl>
#>  1 2010 Q1 x     -0.536 
#>  2 2010 Q2 x     -0.0840
#>  3 2010 Q3 x     -2.01  
#>  4 2010 Q4 x     -0.513 
#>  5 2011 Q1 x      0.102 
#>  6 2011 Q2 x     -0.0562
#>  7 2011 Q3 x     -2.43  
#>  8 2011 Q4 x     -0.626 
#>  9 2012 Q1 x     -0.876 
#> 10 2012 Q2 x      0.286 
#> # ℹ 20 more rows
```
