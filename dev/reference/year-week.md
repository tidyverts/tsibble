# Represent year-week based on the ISO 8601 standard (with flexible start day)

**\[stable\]**

Create or coerce using `yearweek()`.

## Usage

``` r
yearweek(x, week_start = getOption("lubridate.week.start", 1))

make_yearweek(
  year = 1970L,
  week = 1L,
  week_start = getOption("lubridate.week.start", 1)
)

is_yearweek(x)

is_53weeks(year, week_start = getOption("lubridate.week.start", 1))
```

## Arguments

- x:

  Other object.

- week_start:

  An integer between 1 (Monday) and 7 (Sunday) to specify the day on
  which week starts following ISO conventions. Default to 1 (Monday).
  Use `options(lubridate.week.start = 7)` to set this parameter
  globally.

- year, week:

  A vector of numerics give years and weeks.

## Value

year-week (`yearweek`) objects.

`TRUE`/`FALSE` if the year has 53 ISO weeks.

## Display

Use [`format()`](https://rdrr.io/r/base/format.html) to display
`yearweek`, `yearmonth`, and `yearquarter` objects in required formats.
Please see [`strptime()`](https://rdrr.io/r/base/strptime.html) details
for supported conversion specifications.

## See also

[scale_x_yearweek](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
and others for ggplot2 scales

Other index functions:
[`yearmonth()`](https://tsibble.tidyverts.org/dev/reference/year-month.md),
[`yearquarter()`](https://tsibble.tidyverts.org/dev/reference/year-quarter.md)

## Examples

``` r
# coerce POSIXct/Dates to yearweek
x <- seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "1 week")
yearweek(x)
#> <yearweek[53]>
#>  [1] "2015 W53" "2016 W01" "2016 W02" "2016 W03" "2016 W04" "2016 W05"
#>  [7] "2016 W06" "2016 W07" "2016 W08" "2016 W09" "2016 W10" "2016 W11"
#> [13] "2016 W12" "2016 W13" "2016 W14" "2016 W15" "2016 W16" "2016 W17"
#> [19] "2016 W18" "2016 W19" "2016 W20" "2016 W21" "2016 W22" "2016 W23"
#> [25] "2016 W24" "2016 W25" "2016 W26" "2016 W27" "2016 W28" "2016 W29"
#> [31] "2016 W30" "2016 W31" "2016 W32" "2016 W33" "2016 W34" "2016 W35"
#> [37] "2016 W36" "2016 W37" "2016 W38" "2016 W39" "2016 W40" "2016 W41"
#> [43] "2016 W42" "2016 W43" "2016 W44" "2016 W45" "2016 W46" "2016 W47"
#> [49] "2016 W48" "2016 W49" "2016 W50" "2016 W51" "2016 W52"
#> # Week starts on: Monday
yearweek(x, week_start = 7)
#> <yearweek[53]>
#>  [1] "2015 W52" "2015 W01" "2016 W02" "2016 W03" "2016 W04" "2016 W05"
#>  [7] "2016 W06" "2016 W07" "2016 W08" "2016 W09" "2016 W10" "2016 W11"
#> [13] "2016 W12" "2016 W13" "2016 W14" "2016 W15" "2016 W16" "2016 W17"
#> [19] "2016 W18" "2016 W19" "2016 W20" "2016 W21" "2016 W22" "2016 W23"
#> [25] "2016 W24" "2016 W25" "2016 W26" "2016 W27" "2016 W28" "2016 W29"
#> [31] "2016 W30" "2016 W31" "2016 W32" "2016 W33" "2016 W34" "2016 W35"
#> [37] "2016 W36" "2016 W37" "2016 W38" "2016 W39" "2016 W40" "2016 W41"
#> [43] "2016 W42" "2016 W43" "2016 W44" "2016 W45" "2016 W46" "2016 W47"
#> [49] "2016 W48" "2016 W49" "2016 W50" "2016 W51" "2016 W52"
#> # Week starts on: Sunday

# parse characters
yearweek(c("2018 W01", "2018 Wk01", "2018 Week 1"))
#> <yearweek[3]>
#> [1] "2018 W01" "2018 W01" "2018 W01"
#> # Week starts on: Monday

# seq() and arithmetic
wk1 <- yearweek("2017 W50")
wk2 <- yearweek("2018 W12")
seq(from = wk1, to = wk2, by = 2)
#> <yearweek[8]>
#> [1] "2017 W50" "2017 W52" "2018 W02" "2018 W04" "2018 W06" "2018 W08" "2018 W10"
#> [8] "2018 W12"
#> # Week starts on: Monday
wk1 + 0:9
#> <yearweek[10]>
#>  [1] "2017 W50" "2017 W51" "2017 W52" "2018 W01" "2018 W02" "2018 W03"
#>  [7] "2018 W04" "2018 W05" "2018 W06" "2018 W07"
#> # Week starts on: Monday

# display formats
format(c(wk1, wk2), format = "%V/%Y")
#> [1] "50/2017" "12/2018"

make_yearweek(year = 2021, week = 10:11)
#> <yearweek[2]>
#> [1] "2021 W10" "2021 W11"
#> # Week starts on: Monday
make_yearweek(year = 2020:2021, week = 10:11)
#> <yearweek[2]>
#> [1] "2020 W10" "2021 W11"
#> # Week starts on: Monday

is_53weeks(2015:2016)
#> [1]  TRUE FALSE
is_53weeks(1969)
#> [1] FALSE
is_53weeks(1969, week_start = 7)
#> [1] TRUE
```
