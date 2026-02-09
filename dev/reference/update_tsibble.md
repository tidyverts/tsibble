# Update key and index for a tsibble

Update key and index for a tsibble

## Usage

``` r
update_tsibble(
  x,
  key,
  index,
  regular = is_regular(x),
  validate = TRUE,
  .drop = key_drop_default(x)
)
```

## Arguments

- x:

  A tsibble.

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

## Details

Unspecified arguments will inherit the attributes from `x`.

## Examples

``` r
# update index
library(dplyr)
pedestrian %>%
  group_by_key() %>%
  mutate(Hour_Since = Date_Time - min(Date_Time)) %>%
  update_tsibble(index = Hour_Since)
#> # A tsibble: 66,037 x 6 [1h]
#> # Key:       Sensor [4]
#> # Groups:    Sensor [4]
#>    Sensor         Date_Time           Date        Time Count Hour_Since
#>    <chr>          <dttm>              <date>     <int> <int> <drtn>    
#>  1 Birrarung Marr 2015-01-01 00:00:00 2015-01-01     0  1630     0 secs
#>  2 Birrarung Marr 2015-01-01 01:00:00 2015-01-01     1   826  3600 secs
#>  3 Birrarung Marr 2015-01-01 02:00:00 2015-01-01     2   567  7200 secs
#>  4 Birrarung Marr 2015-01-01 03:00:00 2015-01-01     3   264 10800 secs
#>  5 Birrarung Marr 2015-01-01 04:00:00 2015-01-01     4   139 14400 secs
#>  6 Birrarung Marr 2015-01-01 05:00:00 2015-01-01     5    77 18000 secs
#>  7 Birrarung Marr 2015-01-01 06:00:00 2015-01-01     6    44 21600 secs
#>  8 Birrarung Marr 2015-01-01 07:00:00 2015-01-01     7    56 25200 secs
#>  9 Birrarung Marr 2015-01-01 08:00:00 2015-01-01     8   113 28800 secs
#> 10 Birrarung Marr 2015-01-01 09:00:00 2015-01-01     9   166 32400 secs
#> # ℹ 66,027 more rows

# update key: drop the variable "State" from the key
tourism %>%
  update_tsibble(key = c(Purpose, Region))
#> # A tsibble: 24,320 x 5 [1Q]
#> # Key:       Purpose, Region [304]
#>    Quarter Region   State           Purpose  Trips
#>      <qtr> <chr>    <chr>           <chr>    <dbl>
#>  1 1998 Q1 Adelaide South Australia Business  135.
#>  2 1998 Q2 Adelaide South Australia Business  110.
#>  3 1998 Q3 Adelaide South Australia Business  166.
#>  4 1998 Q4 Adelaide South Australia Business  127.
#>  5 1999 Q1 Adelaide South Australia Business  137.
#>  6 1999 Q2 Adelaide South Australia Business  200.
#>  7 1999 Q3 Adelaide South Australia Business  169.
#>  8 1999 Q4 Adelaide South Australia Business  134.
#>  9 2000 Q1 Adelaide South Australia Business  154.
#> 10 2000 Q2 Adelaide South Australia Business  169.
#> # ℹ 24,310 more rows
```
