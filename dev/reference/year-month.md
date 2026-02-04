# Represent year-month

**\[stable\]**

Create or coerce using `yearmonth()`.

## Usage

``` r
yearmonth(x, ...)

make_yearmonth(year = 1970L, month = 1L)

# S3 method for class 'character'
yearmonth(x, format = NULL, ...)

is_yearmonth(x)
```

## Arguments

- x:

  Other object.

- ...:

  Further arguments to methods.

- year, month:

  A vector of numerics give years and months.

- format:

  A vector of strings to specify additional formats of `x` (e.g.
  `%Y%m`), if a warning or an error occurs.

## Value

year-month (`yearmonth`) objects.

## Display

Use [`format()`](https://rdrr.io/r/base/format.html) to display
`yearweek`, `yearmonth`, and `yearquarter` objects in required formats.
Please see [`strptime()`](https://rdrr.io/r/base/strptime.html) details
for supported conversion specifications.

## See also

[scale_x_yearmonth](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
and others for ggplot2 scales

Other index functions:
[`yearquarter()`](https://tsibble.tidyverts.org/dev/reference/year-quarter.md),
[`yearweek()`](https://tsibble.tidyverts.org/dev/reference/year-week.md)

## Examples

``` r
# coerce POSIXct/Dates to yearmonth
x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 month")
yearmonth(x)
#> <yearmonth[12]>
#>  [1] "2016 Jan" "2016 Feb" "2016 Mar" "2016 Apr" "2016 May" "2016 Jun"
#>  [7] "2016 Jul" "2016 Aug" "2016 Sep" "2016 Oct" "2016 Nov" "2016 Dec"

# parse characters
yearmonth(c("2018 Jan", "2018-01", "2018 January"))
#> <yearmonth[3]>
#> [1] "2018 Jan" "2018 Jan" "2018 Jan"

# seq() and arithmetic
mth <- yearmonth("2017-11")
seq(mth, length.out = 10, by = 1) # by 1 month
#> <yearmonth[10]>
#>  [1] "2017 Nov" "2017 Dec" "2018 Jan" "2018 Feb" "2018 Mar" "2018 Apr"
#>  [7] "2018 May" "2018 Jun" "2018 Jul" "2018 Aug"
mth + 0:9
#> <yearmonth[10]>
#>  [1] "2017 Nov" "2017 Dec" "2018 Jan" "2018 Feb" "2018 Mar" "2018 Apr"
#>  [7] "2018 May" "2018 Jun" "2018 Jul" "2018 Aug"

# display formats
format(mth, format = "%y %m")
#> [1] "17 11"

# units since 1970 Jan
as.double(yearmonth("1969 Jan") + 0:24)
#>  [1] -12 -11 -10  -9  -8  -7  -6  -5  -4  -3  -2  -1   0   1   2   3   4   5   6
#> [20]   7   8   9  10  11  12

make_yearmonth(year = 2021, month = 10:11)
#> <yearmonth[2]>
#> [1] "2021 Oct" "2021 Nov"
make_yearmonth(year = 2020:2021, month = 10:11)
#> <yearmonth[2]>
#> [1] "2020 Oct" "2021 Nov"
```
