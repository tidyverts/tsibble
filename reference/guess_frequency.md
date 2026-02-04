# Guess a time frequency from other index objects

**\[stable\]**

A possible frequency passed to the
[`ts()`](https://rdrr.io/r/stats/ts.html) function

## Usage

``` r
guess_frequency(x)
```

## Arguments

- x:

  An index object including "yearmonth", "yearquarter", "Date" and
  others.

## Details

If a series of observations are collected more frequently than weekly,
it is more likely to have multiple seasonalities. This function returns
a frequency value at its smallest. For example, hourly data would have
daily, weekly and annual frequencies of 24, 168 and 8766 respectively,
and hence it gives 24.

## References

<https://robjhyndman.com/hyndsight/seasonal-periods/>

## Examples

``` r
guess_frequency(yearquarter("2016 Q1") + 0:7)
#> [1] 4
guess_frequency(yearmonth("2016 Jan") + 0:23)
#> [1] 12
guess_frequency(seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by = 1))
#> [1] 7
guess_frequency(seq(
  as.POSIXct("2017-01-01 00:00"), as.POSIXct("2017-01-10 23:00"),
  by = "1 hour"
))
#> [1] 24
```
