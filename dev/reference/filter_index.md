# A shorthand for filtering time index for a tsibble

This shorthand respects time zones and encourages compact expressions.

## Usage

``` r
filter_index(.data, ..., .preserve = FALSE)
```

## Arguments

- .data:

  A tsibble.

- ...:

  Formulas that specify start and end periods (inclusive), or strings.

  - `~ end` or `. ~ end`: from the very beginning to a specified ending
    period.

  - `start ~ end`: from specified beginning to ending periods.

  - `start ~ .`: from a specified beginning to the very end of the data.
    Supported index type: `POSIXct` (to seconds), `Date`, `yearweek`,
    `yearmonth`/`yearmon`, `yearquarter`/`yearqtr`, `hms`/`difftime` &
    `numeric`.

- .preserve:

  Relevant when the `.data` input is grouped. If `.preserve = FALSE`
  (the default), the grouping structure is recalculated based on the
  resulting data, otherwise the grouping is kept as is.

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

[time_in](https://tsibble.tidyverts.org/dev/reference/time_in.md) for a
vector of time index

## Examples

``` r
# from the starting time to the end of Feb, 2015
pedestrian %>%
  filter_index(~ "2015-02")
#> # A tsibble: 4,536 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
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
#> # ℹ 4,526 more rows

# entire Feb 2015, & from the beginning of Aug 2016 to the end
pedestrian %>%
  filter_index("2015-02", "2016-08" ~ .)
#> # A tsibble: 16,244 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor         Date_Time           Date        Time Count
#>    <chr>          <dttm>              <date>     <int> <int>
#>  1 Birrarung Marr 2015-02-01 00:00:00 2015-02-01     0   178
#>  2 Birrarung Marr 2015-02-01 01:00:00 2015-02-01     1    39
#>  3 Birrarung Marr 2015-02-01 02:00:00 2015-02-01     2    41
#>  4 Birrarung Marr 2015-02-01 03:00:00 2015-02-01     3    32
#>  5 Birrarung Marr 2015-02-01 04:00:00 2015-02-01     4    33
#>  6 Birrarung Marr 2015-02-01 05:00:00 2015-02-01     5    39
#>  7 Birrarung Marr 2015-02-01 06:00:00 2015-02-01     6    45
#>  8 Birrarung Marr 2015-02-01 07:00:00 2015-02-01     7    45
#>  9 Birrarung Marr 2015-02-01 08:00:00 2015-02-01     8    96
#> 10 Birrarung Marr 2015-02-01 09:00:00 2015-02-01     9   116
#> # ℹ 16,234 more rows

# multiple time windows
pedestrian %>%
  filter_index(~"2015-02", "2015-08" ~ "2015-09", "2015-12" ~ "2016-02")
#> # A tsibble: 19,008 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
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
#> # ℹ 18,998 more rows

# entire 2015
pedestrian %>%
  filter_index("2015")
#> # A tsibble: 32,276 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
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
#> # ℹ 32,266 more rows

# specific
pedestrian %>%
  filter_index("2015-03-23" ~ "2015-10")
#> # A tsibble: 20,180 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor         Date_Time           Date        Time Count
#>    <chr>          <dttm>              <date>     <int> <int>
#>  1 Birrarung Marr 2015-03-23 00:00:00 2015-03-23     0    39
#>  2 Birrarung Marr 2015-03-23 01:00:00 2015-03-23     1    24
#>  3 Birrarung Marr 2015-03-23 02:00:00 2015-03-23     2     1
#>  4 Birrarung Marr 2015-03-23 03:00:00 2015-03-23     3     3
#>  5 Birrarung Marr 2015-03-23 04:00:00 2015-03-23     4    16
#>  6 Birrarung Marr 2015-03-23 05:00:00 2015-03-23     5    36
#>  7 Birrarung Marr 2015-03-23 06:00:00 2015-03-23     6   178
#>  8 Birrarung Marr 2015-03-23 07:00:00 2015-03-23     7   462
#>  9 Birrarung Marr 2015-03-23 08:00:00 2015-03-23     8   756
#> 10 Birrarung Marr 2015-03-23 09:00:00 2015-03-23     9   289
#> # ℹ 20,170 more rows
pedestrian %>%
  filter_index("2015-03-23" ~ "2015-10-31")
#> # A tsibble: 20,180 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor         Date_Time           Date        Time Count
#>    <chr>          <dttm>              <date>     <int> <int>
#>  1 Birrarung Marr 2015-03-23 00:00:00 2015-03-23     0    39
#>  2 Birrarung Marr 2015-03-23 01:00:00 2015-03-23     1    24
#>  3 Birrarung Marr 2015-03-23 02:00:00 2015-03-23     2     1
#>  4 Birrarung Marr 2015-03-23 03:00:00 2015-03-23     3     3
#>  5 Birrarung Marr 2015-03-23 04:00:00 2015-03-23     4    16
#>  6 Birrarung Marr 2015-03-23 05:00:00 2015-03-23     5    36
#>  7 Birrarung Marr 2015-03-23 06:00:00 2015-03-23     6   178
#>  8 Birrarung Marr 2015-03-23 07:00:00 2015-03-23     7   462
#>  9 Birrarung Marr 2015-03-23 08:00:00 2015-03-23     8   756
#> 10 Birrarung Marr 2015-03-23 09:00:00 2015-03-23     9   289
#> # ℹ 20,170 more rows
pedestrian %>%
  filter_index("2015-03-23 10" ~ "2015-10-31 12")
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
```
