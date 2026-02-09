# Coerce to a tsibble object

**\[stable\]**

## Usage

``` r
as_tsibble(
  x,
  key = NULL,
  index,
  regular = TRUE,
  validate = TRUE,
  .drop = TRUE,
  ...
)

# S3 method for class 'ts'
as_tsibble(x, ..., tz = "UTC")

# S3 method for class 'mts'
as_tsibble(x, ..., tz = "UTC", pivot_longer = TRUE)
```

## Arguments

- x:

  Other objects to be coerced to a tsibble (`tbl_ts`).

- key:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Variable(s) that uniquely determine time indices. `NULL` for an empty
  key, unquoted column names (e.g. `x`) for a single variable, and
  [`c()`](https://rdrr.io/r/base/c.html) for multiple variables (e.g.
  `c(x, y)`). This argument also supports [tidy-select
  expressions](https://tidyselect.r-lib.org/reference/language.html),
  e.g.
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html),
  [`dplyr::all_of()`](https://dplyr.tidyverse.org/reference/reexports.html).

- index:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A variable that contains time indices. This is commonly an unquoted
  column name (e.g. `t`), but it can also be a [tidy-select
  expression](https://tidyselect.r-lib.org/reference/language.html).

- regular:

  Regular time interval (`TRUE`) or irregular (`FALSE`). The interval is
  determined by the greatest common divisor of the index column, if
  `TRUE`.

- validate:

  `TRUE` suggests to verify that each key or each combination of key
  variables leads to unique time indices (i.e. a valid tsibble). If you
  are sure that it's a valid input, specify `FALSE` to skip the checks.

- .drop:

  If `TRUE`, empty key groups are dropped.

- ...:

  Other arguments passed on to individual methods.

- tz:

  Time zone. May be useful when a `ts` object is more frequent than
  daily.

- pivot_longer:

  `TRUE` gives a "longer" form of the data, otherwise as is.

## Value

A tsibble object.

## Details

A tsibble is sorted by its key first and index.

## Index

An extensive range of indices are supported by tsibble:

- native time classes in R (such as `Date`, `POSIXct`, and `difftime`)

- tsibble's new additions (such as
  [yearweek](https://tsibble.tidyverts.org/reference/year-week.md),
  [yearmonth](https://tsibble.tidyverts.org/reference/year-month.md),
  and
  [yearquarter](https://tsibble.tidyverts.org/reference/year-quarter.md)).

- other commonly-used classes: `ordered`,
  [`hms::hms`](https://hms.tidyverse.org/reference/hms.html),
  [`lubridate::period`](https://lubridate.tidyverse.org/reference/period.html),
  and
  [`nanotime::nanotime`](https://eddelbuettel.github.io/nanotime/man/nanotime.html).

For a `tbl_ts` of regular interval, a choice of index representation has
to be made. For example, a monthly data should correspond to time index
created by
[yearmonth](https://tsibble.tidyverts.org/reference/year-month.md),
instead of `Date` or `POSIXct`. Because months in a year ensures the
regularity, 12 months every year. However, if using `Date`, a month
containing days ranges from 28 to 31 days, which results in irregular
time space. This is also applicable to year-week and year-quarter.

Tsibble supports arbitrary index classes, as long as they can be ordered
from past to future. To support a custom class, you need to define
[`index_valid()`](https://tsibble.tidyverts.org/reference/index_valid.md)
for the class and calculate the interval through
[`interval_pull()`](https://tsibble.tidyverts.org/reference/interval-pull.md).

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

The [interval](https://tsibble.tidyverts.org/reference/regular.md)
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

[tsibble](https://tsibble.tidyverts.org/reference/tsibble.md)

## Examples

``` r
# coerce tibble to tsibble w/o a key
tbl1 <- tibble(
  date = as.Date("2017-01-01") + 0:9,
  value = rnorm(10)
)
as_tsibble(tbl1)
#> Using `date` as index variable.
#> # A tsibble: 10 x 2 [1D]
#>    date          value
#>    <date>        <dbl>
#>  1 2017-01-01 -2.44   
#>  2 2017-01-02 -0.00557
#>  3 2017-01-03  0.622  
#>  4 2017-01-04  1.15   
#>  5 2017-01-05 -1.82   
#>  6 2017-01-06 -0.247  
#>  7 2017-01-07 -0.244  
#>  8 2017-01-08 -0.283  
#>  9 2017-01-09 -0.554  
#> 10 2017-01-10  0.629  
# supply the index to suppress the message
as_tsibble(tbl1, index = date)
#> # A tsibble: 10 x 2 [1D]
#>    date          value
#>    <date>        <dbl>
#>  1 2017-01-01 -2.44   
#>  2 2017-01-02 -0.00557
#>  3 2017-01-03  0.622  
#>  4 2017-01-04  1.15   
#>  5 2017-01-05 -1.82   
#>  6 2017-01-06 -0.247  
#>  7 2017-01-07 -0.244  
#>  8 2017-01-08 -0.283  
#>  9 2017-01-09 -0.554  
#> 10 2017-01-10  0.629  

# coerce tibble to tsibble with a single variable for key
# use `yearquarter()` to represent quarterly data
tbl2 <- tibble(
  qtr = rep(yearquarter("2010 Q1") + 0:9, 3),
  group = rep(c("x", "y", "z"), each = 10),
  value = rnorm(30)
)
# "qtr" is automatically considered as the index var
as_tsibble(tbl2, key = group)
#> Using `qtr` as index variable.
#> # A tsibble: 30 x 3 [1Q]
#> # Key:       group [3]
#>        qtr group   value
#>      <qtr> <chr>   <dbl>
#>  1 2010 Q1 x      2.07  
#>  2 2010 Q2 x     -1.63  
#>  3 2010 Q3 x      0.512 
#>  4 2010 Q4 x     -1.86  
#>  5 2011 Q1 x     -0.522 
#>  6 2011 Q2 x     -0.0526
#>  7 2011 Q3 x      0.543 
#>  8 2011 Q4 x     -0.914 
#>  9 2012 Q1 x      0.468 
#> 10 2012 Q2 x      0.363 
#> # ℹ 20 more rows
as_tsibble(tbl2, key = group, index = qtr)
#> # A tsibble: 30 x 3 [1Q]
#> # Key:       group [3]
#>        qtr group   value
#>      <qtr> <chr>   <dbl>
#>  1 2010 Q1 x      2.07  
#>  2 2010 Q2 x     -1.63  
#>  3 2010 Q3 x      0.512 
#>  4 2010 Q4 x     -1.86  
#>  5 2011 Q1 x     -0.522 
#>  6 2011 Q2 x     -0.0526
#>  7 2011 Q3 x      0.543 
#>  8 2011 Q4 x     -0.914 
#>  9 2012 Q1 x      0.468 
#> 10 2012 Q2 x      0.363 
#> # ℹ 20 more rows

# create a tsibble with multiple variables for key
# use `yearmonth()` to represent monthly data
tbl3 <- tibble(
  mth = rep(yearmonth("2010 Jan") + 0:8, each = 3),
  xyz = rep(c("x", "y", "z"), each = 9),
  abc = rep(letters[1:3], times = 9),
  value = rnorm(27)
)
as_tsibble(tbl3, key = c(xyz, abc))
#> Using `mth` as index variable.
#> # A tsibble: 27 x 4 [1M]
#> # Key:       xyz, abc [9]
#>         mth xyz   abc     value
#>       <mth> <chr> <chr>   <dbl>
#>  1 2010 Jan x     a     -0.0500
#>  2 2010 Feb x     a      2.76  
#>  3 2010 Mar x     a      0.118 
#>  4 2010 Jan x     b     -0.251 
#>  5 2010 Feb x     b      0.0465
#>  6 2010 Mar x     b     -1.91  
#>  7 2010 Jan x     c      0.445 
#>  8 2010 Feb x     c      0.578 
#>  9 2010 Mar x     c      0.862 
#> 10 2010 Apr y     a     -0.243 
#> # ℹ 17 more rows
# coerce ts to tsibble
as_tsibble(AirPassengers)
#> # A tsibble: 144 x 2 [1M]
#>       index value
#>       <mth> <dbl>
#>  1 1949 Jan   112
#>  2 1949 Feb   118
#>  3 1949 Mar   132
#>  4 1949 Apr   129
#>  5 1949 May   121
#>  6 1949 Jun   135
#>  7 1949 Jul   148
#>  8 1949 Aug   148
#>  9 1949 Sep   136
#> 10 1949 Oct   119
#> # ℹ 134 more rows
as_tsibble(sunspot.year)
#> # A tsibble: 289 x 2 [1Y]
#>    index value
#>    <dbl> <dbl>
#>  1  1700     5
#>  2  1701    11
#>  3  1702    16
#>  4  1703    23
#>  5  1704    36
#>  6  1705    58
#>  7  1706    29
#>  8  1707    20
#>  9  1708    10
#> 10  1709     8
#> # ℹ 279 more rows
as_tsibble(sunspot.month)
#> # A tsibble: 3,310 x 2 [1M]
#>       index value
#>       <mth> <dbl>
#>  1 1749 Jan  96.7
#>  2 1749 Feb 104. 
#>  3 1749 Mar 117. 
#>  4 1749 Apr  92.8
#>  5 1749 May 142. 
#>  6 1749 Jun 139. 
#>  7 1749 Jul 158  
#>  8 1749 Aug 110. 
#>  9 1749 Sep 126. 
#> 10 1749 Oct 126. 
#> # ℹ 3,300 more rows
as_tsibble(austres)
#> # A tsibble: 89 x 2 [1Q]
#>      index  value
#>      <qtr>  <dbl>
#>  1 1971 Q2 13067.
#>  2 1971 Q3 13130.
#>  3 1971 Q4 13198.
#>  4 1972 Q1 13254.
#>  5 1972 Q2 13304.
#>  6 1972 Q3 13354.
#>  7 1972 Q4 13409.
#>  8 1973 Q1 13459.
#>  9 1973 Q2 13504.
#> 10 1973 Q3 13553.
#> # ℹ 79 more rows
# coerce mts to tsibble
z <- ts(matrix(rnorm(300), 100, 3), start = c(1961, 1), frequency = 12)
as_tsibble(z)
#> # A tsibble: 300 x 3 [1M]
#> # Key:       key [3]
#>       index key       value
#>       <mth> <chr>     <dbl>
#>  1 1961 Jan Series 1 -1.70 
#>  2 1961 Feb Series 1 -1.47 
#>  3 1961 Mar Series 1  0.284
#>  4 1961 Apr Series 1  1.34 
#>  5 1961 May Series 1  0.237
#>  6 1961 Jun Series 1  1.32 
#>  7 1961 Jul Series 1  0.524
#>  8 1961 Aug Series 1  0.607
#>  9 1961 Sep Series 1 -0.110
#> 10 1961 Oct Series 1  0.172
#> # ℹ 290 more rows
as_tsibble(z, pivot_longer = FALSE)
#> # A tsibble: 100 x 4 [1M]
#>       index `Series 1` `Series 2` `Series 3`
#>       <mth>      <dbl>      <dbl>      <dbl>
#>  1 1961 Jan     -1.70       0.518    0.240  
#>  2 1961 Feb     -1.47      -0.103    0.0609 
#>  3 1961 Mar      0.284     -0.974   -2.18   
#>  4 1961 Apr      1.34       1.27    -0.118  
#>  5 1961 May      0.237      0.961    0.112  
#>  6 1961 Jun      1.32       0.769    0.00789
#>  7 1961 Jul      0.524      1.04     1.88   
#>  8 1961 Aug      0.607     -0.474    2.16   
#>  9 1961 Sep     -0.110     -1.28     0.710  
#> 10 1961 Oct      0.172     -0.306    0.767  
#> # ℹ 90 more rows
```
