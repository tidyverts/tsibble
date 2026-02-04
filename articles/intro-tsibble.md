# Introduction to tsibble

The **tsibble** package extends the
[tidyverse](https://www.tidyverse.org) to temporal data. Built on top of
the [tibble](https://tibble.tidyverse.org/), a tsibble (or `tbl_ts`) is
a data- and model-oriented object. Compared to the conventional time
series objects in R, for example `ts`, `zoo`, and `xts`, the tsibble
preserves time indices as the essential data column and makes
heterogeneous data structures possible. Beyond the tibble-like
representation, **key** comprised of single or multiple variables is
introduced to uniquely identify observational units over time
(**index**). The tsibble package aims at managing temporal data and
getting analysis done in a fluent workflow.

## Contextual semantics: index and key

[`tsibble()`](https://tsibble.tidyverts.org/reference/tsibble.md)
creates a tsibble object, and
[`as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.md)
is an S3 method to coerce other objects to a tsibble. An object that a
vector/matrix underlies, such as `ts` and `mts`, can be automated to a
tsibble using
[`as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.md)
without any specification. If it is a tibble or data frame,
[`as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.md)
requires a little more setup in order to declare the index and key
variables.

``` r
library(dplyr)
library(lubridate)
library(tsibble)
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather
#> # A tibble: 26,115 × 5
#>   origin time_hour            temp humid precip
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>
#> 1 EWR    2013-01-01 01:00:00  39.0  59.4      0
#> 2 EWR    2013-01-01 02:00:00  39.0  61.6      0
#> 3 EWR    2013-01-01 03:00:00  39.0  64.4      0
#> 4 EWR    2013-01-01 04:00:00  39.9  62.2      0
#> 5 EWR    2013-01-01 05:00:00  39.0  64.4      0
#> # ℹ 26,110 more rows
```

The `weather` data included in the package `nycflights13` contains the
hourly meteorological records (such as temperature, humid and
precipitation) over the year of 2013 at three stations (i.e. JFK, LGA
and EWR) in New York City. Since the `time_hour` is the only column
involving the timestamps,
[`as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.md)
defaults it to the index variable; alternatively, the index can be
specified by the argument `index = time_hour` to disable the verbose
message.

Except for index, a tsibble requires “key”, which defines subjects or
individuals measured over time. In this example, the `origin` variable
is the identifier, which is passed to the argument `key` in
[`as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.md).
**Each observation should be uniquely identified by index and key** in a
valid tsibble. Others—`temp`, `humid` and `precip`—are referred to as
measured variables. When creating a tsibble, the key will be sorted
first, followed by arranging time from past to recent.

``` r
weather_tsbl <- as_tsibble(weather, key = origin)
#> Using `time_hour` as index variable.
weather_tsbl
#> # A tsibble: 26,115 x 5 [1h] <America/New_York>
#> # Key:       origin [3]
#>   origin time_hour            temp humid precip
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>
#> 1 EWR    2013-01-01 01:00:00  39.0  59.4      0
#> 2 EWR    2013-01-01 02:00:00  39.0  61.6      0
#> 3 EWR    2013-01-01 03:00:00  39.0  64.4      0
#> 4 EWR    2013-01-01 04:00:00  39.9  62.2      0
#> 5 EWR    2013-01-01 05:00:00  39.0  64.4      0
#> # ℹ 26,110 more rows
```

An interval is automatically obtained based on the corresponding time
representation:

- `integer`/`numeric`/`ordered`: either “unit” or “year” (`Y`)
- `yearquarter`/`yearqtr`: “quarter” (`Q`)
- `yearmonth`/`yearmon`: “month” (`M`)
- `yearweek`: “week” (`W`)
- `Date`: “day” (`D`)
- `difftime`: “week” (`W`), “day” (D), “hour” (`h`), “minute” (`m`),
  “second” (`s`)
- `POSIXct`/`hms`: “hour” (`h`), “minute” (`m`), “second” (`s`),
  “millisecond” (`us`), “microsecond” (`ms`)
- `nanotime`: “nanosecond” (`ns`)

That is, a tsibble of monthly intervals expects the
`yearmonth`/`yearmon` class in the index column. Neither `Date` nor
`POSIXct` gives a monthly tsibble.

The print display is data-centric and contextually informative, such as
data dimension, time interval, and the number of time-based units. Above
displays the `weather_tsbl` its one-hour interval (`[1h]`) and the
`origin [3]` as the key along with three time series in the table.

## Data pipeline

This tidy data representation most naturally supports thinking of
operations on the data as building blocks, forming part of a “data
pipeline” in time-based context. Users who are familiar with tidyverse
would find it easier to perform common temporal analysis tasks. For
example,
[`index_by()`](https://tsibble.tidyverts.org/reference/index-by.md) is
the counterpart of
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) in
temporal context, but it only groups the time index.
[`index_by()`](https://tsibble.tidyverts.org/reference/index-by.md) +
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html) is
used to summarise daily highs and lows at each station. As a result, the
index is updated to the `date` with one-day interval from the index
`time_hour`; two new variables are created and computed for daily
maximum and minimum temperatures.

``` r
weather_tsbl %>%
  group_by_key() %>%
  index_by(date = ~ as_date(.)) %>% 
  summarise(
    temp_high = max(temp, na.rm = TRUE),
    temp_low = min(temp, na.rm = TRUE)
  )
#> # A tsibble: 1,092 x 4 [1D]
#> # Key:       origin [3]
#>   origin date       temp_high temp_low
#>   <chr>  <date>         <dbl>    <dbl>
#> 1 EWR    2013-01-01      41       28.0
#> 2 EWR    2013-01-02      34.0     24.1
#> 3 EWR    2013-01-03      34.0     26.1
#> 4 EWR    2013-01-04      39.9     28.9
#> 5 EWR    2013-01-05      44.1     32  
#> # ℹ 1,087 more rows
```

## Irregular time interval

Note that the tsibble handles regularly-spaced temporal data well, from
seconds to years based on its time representation (see
[`?tsibble`](https://tsibble.tidyverts.org/reference/tsibble.md)). The
option `regular`, by default, is set to `TRUE` in
[`as_tsibble()`](https://tsibble.tidyverts.org/reference/as-tsibble.md).
Specify `regular` to `FALSE` to create a tsibble for the data collected
at irregular time interval. Below shows the scheduled date time of the
flights in New York:

``` r
flights <- nycflights13::flights %>%
  mutate(sched_dep_datetime = 
    make_datetime(year, month, day, hour, minute, tz = "America/New_York"))
```

The key contains columns `carrier` and `flight` to identify
observational units over time, from a passenger’s point of view. With
`regular = FALSE`, it turns to an irregularly-spaced tsibble, where
`[!]` highlights the irregularity.

``` r
flights_tsbl <- flights %>%
  as_tsibble(
    key = c(carrier, flight), 
    index = sched_dep_datetime, 
    regular = FALSE
  )
flights_tsbl
#> # A tsibble: 336,776 x 20 [!] <America/New_York>
#> # Key:       carrier, flight [5,725]
#>    year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
#>   <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
#> 1  2013    11     3     1531           1540        -9     1653           1725
#> 2  2013    11     4     1539           1540        -1     1712           1725
#> 3  2013    11     5     1548           1540         8     1708           1725
#> 4  2013    11     6     1535           1540        -5     1657           1725
#> 5  2013    11     7     1549           1540         9     1733           1725
#> # ℹ 336,771 more rows
#> # ℹ 12 more variables: arr_delay <dbl>, carrier <chr>, flight <int>,
#> #   tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>, distance <dbl>,
#> #   hour <dbl>, minute <dbl>, time_hour <dttm>, sched_dep_datetime <dttm>
```

To regularise an irregular tsibble, it can be achieved with
[`index_by()`](https://tsibble.tidyverts.org/reference/index-by.md) +
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
