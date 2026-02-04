# Scan a tsibble for implicit missing observations

Scan a tsibble for implicit missing observations

## Usage

``` r
scan_gaps(.data, .full = FALSE, .start = NULL, .end = NULL)
```

## Arguments

- .data:

  A tsibble.

- .full:

  - `FALSE` inserts `NA` for each keyed unit within its own period.

  - `TRUE` fills `NA` over the entire time span of the data (a.k.a.
    fully balanced panel).

  - [`start()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    starting point (i.e. `min(<index>)`) across units.

  - [`end()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    ending point (i.e. `max(<index>)`) across units.

- .start, .end:

  Set custom starting/ending time that allows to expand the existing
  time spans.

## See also

Other implicit gaps handling:
[`count_gaps()`](https://tsibble.tidyverts.org/reference/count_gaps.md),
[`fill_gaps()`](https://tsibble.tidyverts.org/reference/fill_gaps.md),
[`has_gaps()`](https://tsibble.tidyverts.org/reference/has_gaps.md)

## Examples

``` r
scan_gaps(pedestrian)
#> # A tsibble: 3,011 x 2 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor         Date_Time          
#>    <chr>          <dttm>             
#>  1 Birrarung Marr 2015-04-05 02:00:00
#>  2 Birrarung Marr 2015-05-07 00:00:00
#>  3 Birrarung Marr 2015-05-07 01:00:00
#>  4 Birrarung Marr 2015-05-07 02:00:00
#>  5 Birrarung Marr 2015-05-07 03:00:00
#>  6 Birrarung Marr 2015-05-07 04:00:00
#>  7 Birrarung Marr 2015-05-07 05:00:00
#>  8 Birrarung Marr 2015-05-07 06:00:00
#>  9 Birrarung Marr 2015-05-07 07:00:00
#> 10 Birrarung Marr 2015-05-07 08:00:00
#> # ℹ 3,001 more rows
```
