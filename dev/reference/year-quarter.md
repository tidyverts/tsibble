# Represent year-quarter

**\[stable\]**

Create or coerce using `yearquarter()`.

## Usage

``` r
yearquarter(x, fiscal_start = 1)

make_yearquarter(year = 1970L, quarter = 1L, fiscal_start = 1)

is_yearquarter(x)

fiscal_year(x)
```

## Arguments

- x:

  Other object.

- fiscal_start:

  numeric indicating the starting month of a fiscal year.

- year, quarter:

  A vector of numerics give years and quarters.

## Value

year-quarter (`yearquarter`) objects.

## Display

Use [`format()`](https://rdrr.io/r/base/format.html) to display
`yearweek`, `yearmonth`, and `yearquarter` objects in required formats.
Please see [`strptime()`](https://rdrr.io/r/base/strptime.html) details
for supported conversion specifications.

## See also

[scale_x_yearquarter](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
and others for ggplot2 scales

Other index functions:
[`yearmonth()`](https://tsibble.tidyverts.org/dev/reference/year-month.md),
[`yearweek()`](https://tsibble.tidyverts.org/dev/reference/year-week.md)

## Examples

``` r
# coerce POSIXct/Dates to yearquarter
x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 quarter")
yearquarter(x)
#> <yearquarter[4]>
#> [1] "2016 Q1" "2016 Q2" "2016 Q3" "2016 Q4"
#> # Year starts on: January
yearquarter(x, fiscal_start = 6)
#> <yearquarter[4]>
#> [1] "2016 Q3" "2016 Q4" "2017 Q1" "2017 Q2"
#> # Year starts on: June

# parse characters
yearquarter(c("2018 Q1", "2018 Qtr1", "2018 Quarter 1"))
#> <yearquarter[3]>
#> [1] "2018 Q1" "2018 Q1" "2018 Q1"
#> # Year starts on: January

# seq() and arithmetic
qtr <- yearquarter("2017 Q1")
seq(qtr, length.out = 10, by = 1) # by 1 quarter
#> <yearquarter[10]>
#>  [1] "2017 Q1" "2017 Q2" "2017 Q3" "2017 Q4" "2018 Q1" "2018 Q2" "2018 Q3"
#>  [8] "2018 Q4" "2019 Q1" "2019 Q2"
#> # Year starts on: January
qtr + 0:9
#> <yearquarter[10]>
#>  [1] "2017 Q1" "2017 Q2" "2017 Q3" "2017 Q4" "2018 Q1" "2018 Q2" "2018 Q3"
#>  [8] "2018 Q4" "2019 Q1" "2019 Q2"
#> # Year starts on: January

# display formats
format(qtr, format = "%y Qtr%q")
#> [1] "17 Qtr1"

make_yearquarter(year = 2021, quarter = 2:3)
#> <yearquarter[2]>
#> [1] "2021 Q2" "2021 Q3"
#> # Year starts on: January
make_yearquarter(year = 2020:2021, quarter = 2:3)
#> <yearquarter[2]>
#> [1] "2020 Q2" "2021 Q3"
#> # Year starts on: January

# `fiscal_year()` helps to extract fiscal year
y <- yearquarter(as.Date("2020-06-01"), fiscal_start = 6)
fiscal_year(y)
#> [1] 2021
lubridate::year(y) # calendar years
#> [1] 2020
```
