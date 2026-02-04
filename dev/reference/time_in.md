# If time falls in the ranges using compact expressions

This function respects time zone and encourages compact expressions.

## Usage

``` r
time_in(x, ...)
```

## Arguments

- x:

  A vector of time index, such as classes `POSIXct`, `Date`, `yearweek`,
  `yearmonth`, `yearquarter`, `hms`/`difftime`, and `numeric`.

- ...:

  Formulas that specify start and end periods (inclusive), or strings.

  - `~ end` or `. ~ end`: from the very beginning to a specified ending
    period.

  - `start ~ end`: from specified beginning to ending periods.

  - `start ~ .`: from a specified beginning to the very end of the data.
    Supported index type: `POSIXct` (to seconds), `Date`, `yearweek`,
    `yearmonth`/`yearmon`, `yearquarter`/`yearqtr`, `hms`/`difftime` &
    `numeric`.

## Value

logical vector

## System Time Zone ("Europe/London")

There is a known issue of an extra hour gained for a machine setting
time zone to "Europe/London", regardless of the time zone associated
with the POSIXct inputs. It relates to *anytime* and *Boost*. Use
[`Sys.timezone()`](https://rdrr.io/r/base/timezones.html) to check if
the system time zone is "Europe/London". It would be recommended to
change the global environment "TZ" to other equivalent names: GB,
GB-Eire, Europe/Belfast, Europe/Guernsey, Europe/Isle_of_Man and
Europe/Jersey as documented in `?Sys.timezone()`, using
`Sys.setenv(TZ = "GB")` for example.

## See also

[filter_index](https://tsibble.tidyverts.org/dev/reference/filter_index.md)
for filtering tsibble

## Examples

``` r
x <- unique(pedestrian$Date_Time)
lgl <- time_in(x, ~"2015-02", "2015-08" ~ "2015-09", "2015-12" ~ "2016-02")
lgl[1:10]
#>  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
# more specific
lgl2 <- time_in(x, "2015-03-23 10" ~ "2015-10-31 12")
lgl2[1:10]
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE

library(dplyr)
pedestrian %>%
  filter(time_in(Date_Time, "2015-03-23 10" ~ "2015-10-31 12"))
#> # A tsibble: 20,107 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor         Date_Time           Date        Time Count
#>    <chr>          <dttm>              <date>     <int> <int>
#>  1 Birrarung Marr 2015-03-23 10:00:00 2015-03-23    10   199
#>  2 Birrarung Marr 2015-03-23 11:00:00 2015-03-23    11   120
#>  3 Birrarung Marr 2015-03-23 12:00:00 2015-03-23    12   317
#>  4 Birrarung Marr 2015-03-23 13:00:00 2015-03-23    13   583
#>  5 Birrarung Marr 2015-03-23 14:00:00 2015-03-23    14   265
#>  6 Birrarung Marr 2015-03-23 15:00:00 2015-03-23    15   275
#>  7 Birrarung Marr 2015-03-23 16:00:00 2015-03-23    16   409
#>  8 Birrarung Marr 2015-03-23 17:00:00 2015-03-23    17   698
#>  9 Birrarung Marr 2015-03-23 18:00:00 2015-03-23    18   546
#> 10 Birrarung Marr 2015-03-23 19:00:00 2015-03-23    19   276
#> # ℹ 20,097 more rows
pedestrian %>%
  filter(time_in(Date_Time, "2015")) %>%
  mutate(Season = ifelse(
    time_in(Date_Time, "2015-03" ~ "2015-08"),
    "Autumn-Winter", "Spring-Summer"
  ))
#> # A tsibble: 32,276 x 6 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor         Date_Time           Date        Time Count Season       
#>    <chr>          <dttm>              <date>     <int> <int> <chr>        
#>  1 Birrarung Marr 2015-01-01 00:00:00 2015-01-01     0  1630 Spring-Summer
#>  2 Birrarung Marr 2015-01-01 01:00:00 2015-01-01     1   826 Spring-Summer
#>  3 Birrarung Marr 2015-01-01 02:00:00 2015-01-01     2   567 Spring-Summer
#>  4 Birrarung Marr 2015-01-01 03:00:00 2015-01-01     3   264 Spring-Summer
#>  5 Birrarung Marr 2015-01-01 04:00:00 2015-01-01     4   139 Spring-Summer
#>  6 Birrarung Marr 2015-01-01 05:00:00 2015-01-01     5    77 Spring-Summer
#>  7 Birrarung Marr 2015-01-01 06:00:00 2015-01-01     6    44 Spring-Summer
#>  8 Birrarung Marr 2015-01-01 07:00:00 2015-01-01     7    56 Spring-Summer
#>  9 Birrarung Marr 2015-01-01 08:00:00 2015-01-01     8   113 Spring-Summer
#> 10 Birrarung Marr 2015-01-01 09:00:00 2015-01-01     9   166 Spring-Summer
#> # ℹ 32,266 more rows
```
