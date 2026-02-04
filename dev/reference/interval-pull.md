# Pull time interval from a vector

**\[stable\]**

Assuming regularly spaced time, the `interval_pull()` returns a list of
time components as the "interval" class.

## Usage

``` r
interval_pull(x)
```

## Arguments

- x:

  A vector of index-like class.

## Value

An "interval" class (a list) includes "year", "quarter", "month",
"week", "day", "hour", "minute", "second", "millisecond", "microsecond",
"nanosecond", "unit".

## Details

Extend tsibble to support custom time indexes by defining S3 generics
[`index_valid()`](https://tsibble.tidyverts.org/dev/reference/index_valid.md)
and `interval_pull()` for them.

## Examples

``` r
x <- seq(as.Date("2017-10-01"), as.Date("2017-10-31"), by = 3)
interval_pull(x)
#> <interval[1]>
#> [1] 3D
```
