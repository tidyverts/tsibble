# Low-level constructor for a tsibble object

`build_tsibble()` creates a `tbl_ts` object with more controls. It is
useful for creating a `tbl_ts` internally inside a function, and it
allows developers to determine if the time needs ordering and the
interval needs calculating.

## Usage

``` r
build_tsibble(
  x,
  key = NULL,
  key_data = NULL,
  index,
  index2 = index,
  ordered = NULL,
  interval = TRUE,
  validate = TRUE,
  .drop = key_drop_default(x)
)
```

## Arguments

- x:

  A `data.frame`, `tbl_df`, `tbl_ts`, or other tabular objects.

- key:

  Variable(s) that uniquely determine time indices. `NULL` for empty
  key, and [`c()`](https://rdrr.io/r/base/c.html) for multiple
  variables. It works with tidy selector (e.g.
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html)).

- key_data:

  A data frame containing key variables and `.rows`. When a data frame
  is supplied, the argument `key` will be ignored.

- index:

  A variable to specify the time index variable.

- index2:

  A candidate of `index` to update the index to a new one when
  [index_by](https://tsibble.tidyverts.org/reference/index-by.md). By
  default, it's identical to `index`.

- ordered:

  The default of `NULL` arranges the key variable(s) first and then
  index from past to future. `TRUE` suggests to skip the ordering as `x`
  in the correct order. `FALSE` checks the ordering and may give a
  warning.

- interval:

  `TRUE` automatically calculates the interval, and `FALSE` for
  irregular interval. Use the specified interval via
  [`new_interval()`](https://tsibble.tidyverts.org/reference/new-interval.md)
  as is.

- validate:

  `TRUE` suggests to verify that each key or each combination of key
  variables leads to unique time indices (i.e. a valid tsibble). If you
  are sure that it's a valid input, specify `FALSE` to skip the checks.

- .drop:

  If `TRUE`, empty key groups are dropped.

## Examples

``` r
# Prepare `pedestrian` to use a new index `Date` ----
pedestrian %>%
  build_tsibble(
    key = !!key_vars(.), index = !!index(.), index2 = Date,
    interval = interval(.)
  )
#> # A tsibble: 66,037 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#> # Groups:    @ Date [731]
#>    Sensor         Date_Time           Date        Time Count
#>    <chr>          <dttm>              <date>     <int> <int>
#>  1 Birrarung Marr 2015-01-01 00:00:00 2015-01-01     0  1630
#>  2 Birrarung Marr 2015-01-01 01:00:00 2015-01-01     1   826
#>  3 Birrarung Marr 2015-01-01 02:00:00 2015-01-01     2   567
#>  4 Birrarung Marr 2015-01-01 03:00:00 2015-01-01     3   264
#>  5 Birrarung Marr 2015-01-01 04:00:00 2015-01-01     4   139
#>  6 Birrarung Marr 2015-01-01 05:00:00 2015-01-01     5    77
#>  7 Birrarung Marr 2015-01-01 06:00:00 2015-01-01     6    44
#>  8 Birrarung Marr 2015-01-01 07:00:00 2015-01-01     7    56
#>  9 Birrarung Marr 2015-01-01 08:00:00 2015-01-01     8   113
#> 10 Birrarung Marr 2015-01-01 09:00:00 2015-01-01     9   166
#> # ℹ 66,027 more rows
```
