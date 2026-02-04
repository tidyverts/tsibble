# Create a tsibble object

**\[stable\]**

## Usage

``` r
tsibble(..., key = NULL, index, regular = TRUE, .drop = TRUE)
```

## Arguments

- ...:

  A set of name-value pairs.

- key:

  Variable(s) that uniquely determine time indices. `NULL` for empty
  key, and [`c()`](https://rdrr.io/r/base/c.html) for multiple
  variables. It works with tidy selector (e.g.
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html)).

- index:

  A variable to specify the time index variable.

- regular:

  Regular time interval (`TRUE`) or irregular (`FALSE`). The interval is
  determined by the greatest common divisor of index column, if `TRUE`.

- .drop:

  If `TRUE`, empty key groups are dropped.

## Value

A tsibble object.

## Details

A tsibble is sorted by its key first and index.

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

## See also

[build_tsibble](https://tsibble.tidyverts.org/dev/reference/build_tsibble.md)

## Examples

``` r
# create a tsibble w/o a key
tsibble(
  date = as.Date("2017-01-01") + 0:9,
  value = rnorm(10)
)
#> Using `date` as index variable.
#> # A tsibble: 10 x 2 [1D]
#>    date         value
#>    <date>       <dbl>
#>  1 2017-01-01 -1.06  
#>  2 2017-01-02 -1.04  
#>  3 2017-01-03 -1.23  
#>  4 2017-01-04  0.229 
#>  5 2017-01-05  0.622 
#>  6 2017-01-06  0.115 
#>  7 2017-01-07  1.50  
#>  8 2017-01-08 -0.574 
#>  9 2017-01-09 -1.17  
#> 10 2017-01-10  0.0540

# create a tsibble with a single variable for key
tsibble(
  qtr = rep(yearquarter("2010 Q1") + 0:9, 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30),
  key = group
)
#> Using `qtr` as index variable.
#> # A tsibble: 30 x 3 [1Q]
#> # Key:       group [3]
#>        qtr group   value
#>      <qtr> <chr>   <dbl>
#>  1 2010 Q1 x      1.22  
#>  2 2010 Q2 x     -0.442 
#>  3 2010 Q3 x      0.746 
#>  4 2010 Q4 x      0.213 
#>  5 2011 Q1 x     -1.33  
#>  6 2011 Q2 x      1.67  
#>  7 2011 Q3 x     -0.126 
#>  8 2011 Q4 x     -2.08  
#>  9 2012 Q1 x      1.67  
#> 10 2012 Q2 x      0.0101
#> # ℹ 20 more rows

# create a tsibble with multiple variables for key
tsibble(
  mth = rep(yearmonth("2010 Jan") + 0:8, each = 3),
  xyz = rep(c("x", "y", "z"), each = 9),
  abc = rep(letters[1:3], times = 9),
  value = rnorm(27),
  key = c(xyz, abc)
)
#> Using `mth` as index variable.
#> # A tsibble: 27 x 4 [1M]
#> # Key:       xyz, abc [9]
#>         mth xyz   abc     value
#>       <mth> <chr> <chr>   <dbl>
#>  1 2010 Jan x     a     -0.387 
#>  2 2010 Feb x     a     -0.0651
#>  3 2010 Mar x     a     -0.555 
#>  4 2010 Jan x     b      0.0726
#>  5 2010 Feb x     b      0.837 
#>  6 2010 Mar x     b      1.40  
#>  7 2010 Jan x     c     -0.383 
#>  8 2010 Feb x     c      1.34  
#>  9 2010 Mar x     c      0.444 
#> 10 2010 Apr y     a      0.169 
#> # ℹ 17 more rows

# create a tsibble containing "key" and "index" as column names
tsibble(!!!list(
  index = rep(yearquarter("2010 Q1") + 0:9, 3),
  key = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30)),
  key = key, index = index
)
#> # A tsibble: 30 x 3 [1Q]
#> # Key:       key [3]
#>      index key     value
#>      <qtr> <chr>   <dbl>
#>  1 2010 Q1 x      0.270 
#>  2 2010 Q2 x     -0.857 
#>  3 2010 Q3 x      1.56  
#>  4 2010 Q4 x     -1.09  
#>  5 2011 Q1 x     -0.0259
#>  6 2011 Q2 x     -1.51  
#>  7 2011 Q3 x     -0.307 
#>  8 2011 Q4 x     -1.88  
#>  9 2012 Q1 x      0.466 
#> 10 2012 Q2 x      0.384 
#> # ℹ 20 more rows
```
