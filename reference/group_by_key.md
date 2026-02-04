# Group by key variables

**\[stable\]**

## Usage

``` r
group_by_key(.data, ..., .drop = key_drop_default(.data))
```

## Arguments

- .data:

  A `tbl_ts` object.

- ...:

  Ignored.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/reference/group_by_drop_default.html)
  for details.

## Examples

``` r
tourism %>%
  group_by_key()
#> # A tsibble: 24,320 x 5 [1Q]
#> # Key:       Region, State, Purpose [304]
#> # Groups:    Region, State, Purpose [304]
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
