# Meta-information of a tsibble

- `interval()` returns an interval of a tsibble.

- `is_regular` checks if a tsibble is spaced at regular time or not.

- `is_ordered` checks if a tsibble is ordered by key and index.

## Usage

``` r
interval(x)

is_regular(x)

is_ordered(x)
```

## Arguments

- x:

  A tsibble object.

## Examples

``` r
interval(pedestrian)
#> Warning: tz(): Don't know how to compute timezone for object of class tbl_ts/tbl_df/tbl/data.frame; returning "UTC".
#> <Interval[0]>
is_regular(pedestrian)
#> [1] TRUE
is_ordered(pedestrian)
#> [1] TRUE
```
