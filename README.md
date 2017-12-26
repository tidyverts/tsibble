
<!-- README.md is generated from README.Rmd. Please edit that file -->
tsibble
=======

[![Travis-CI Build Status](https://travis-ci.org/earowang/tsibble.svg?branch=master)](https://travis-ci.org/earowang/tsibble) [![Coverage Status](https://img.shields.io/codecov/c/github/earowang/tsibble/master.svg)](https://codecov.io/github/earowang/tsibble?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tsibble)](https://cran.r-project.org/package=tsibble)

The **tsibble** package provides a data class of `tbl_ts` to manage temporal-context data frames in a tidy and modern way. A *tsibble* consists of a time index, keys and other measured variables in a data-centric format, which is built on top of the *tibble*.

Installation
------------

You could install the development version from Github using

``` r
# install.packages("devtools")
devtools::install_github("earowang/tsibble", build_vignettes = TRUE)
```

Get started
-----------

### Coerce to a tsibble with `as_tsibble()`

The `weather` data included in the package `nycflights13` is used as an example to illustrate. The "index" variable refers to the `time_hour` containing the date-times, and the "key" is the `origin` as weather stations. **The key together with the index uniquely identifies each observation**, which defines a valid *tsibble*. Others can be considered as measured variables.

``` r
library(tsibble)
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather_tsbl <- as_tsibble(weather, key = id(origin), index = time_hour)
weather_tsbl
#> # A tsibble: 26,130 x 5 [1HOUR]
#> # Keys:      origin
#>   origin           time_hour  temp humid precip
#> *  <chr>              <dttm> <dbl> <dbl>  <dbl>
#> 1    EWR 2013-01-01 11:00:00 37.04 53.97      0
#> 2    EWR 2013-01-01 12:00:00 37.04 53.97      0
#> 3    EWR 2013-01-01 13:00:00 37.94 52.09      0
#> 4    EWR 2013-01-01 14:00:00 37.94 54.51      0
#> 5    EWR 2013-01-01 15:00:00 37.94 57.04      0
#> # ... with 2.612e+04 more rows
```

The **key** is not constrained to a single variable, but expressive of nested and crossed data structures. See `?tsibble` and `vignette("intro-tsibble")` for details.

### `tsummarise()` to summarise over calendar periods

A new verb `tsummarise()` is here to aggregate interested variables over calendar periods. The `tsummarise` goes hand in hand with the index functions including `as.Date()`, `yearmth()`, and `yearqtr()`, as well as other friends from *lubridate*, such as `year()` and `ceiling_date()`. For example, it would be of interest in computing average temperature and total precipitation per month, by applying the `yearmth()` to the hourly time index.

``` r
weather_tsbl %>%
  group_by(origin) %>%
  tsummarise(
    year_month = yearmth(time_hour), # monthly aggregates
    avg_temp = mean(temp, na.rm = TRUE),
    ttl_precip = sum(precip, na.rm = TRUE)
  )
#> # A tsibble: 36 x 4 [1MONTH]
#> # Keys:      origin
#> # Groups:    origin
#>   origin year_month avg_temp ttl_precip
#> *  <chr>      <mth>    <dbl>      <dbl>
#> 1    EWR   2013 Jan 35.45787       2.70
#> 2    EWR   2013 Feb 34.09193       2.76
#> 3    EWR   2013 Mar 39.98992       1.92
#> 4    EWR   2013 Apr 52.75792       1.07
#> 5    EWR   2013 May 62.75508       2.76
#> # ... with 31 more rows
```

The `tsummarise()` can be a useful function for regularising an irregular tsibble.

### Window functions applied to a tsibble: `slide()`, `tile()`, `stretch()`

Time series data commonly get involved in moving window calculations. A set of verbs provided in the *tsibble* allow for different variations of moving windows:

-   `slide()`: sliding window with overlapping observations.
-   `tile()`: tiling window without overlapping observations.
-   `stretch()`: fixing an initial window and expanding more observations.

For example, a moving average of window size 3 is carried out on hourly temperatures for each group (*origin*).

``` r
weather_tsbl %>% 
  select(origin, time_hour, temp) %>% 
  group_by(origin) %>% 
  mutate(temp_mv = slide(temp, mean, size = 3))
#> # A tsibble: 26,130 x 4 [1HOUR]
#> # Keys:      origin
#> # Groups:    origin
#>   origin           time_hour  temp temp_mv
#> *  <chr>              <dttm> <dbl>   <dbl>
#> 1    EWR 2013-01-01 11:00:00 37.04      NA
#> 2    EWR 2013-01-01 12:00:00 37.04      NA
#> 3    EWR 2013-01-01 13:00:00 37.94   37.34
#> 4    EWR 2013-01-01 14:00:00 37.94   37.64
#> 5    EWR 2013-01-01 15:00:00 37.94   37.94
#> # ... with 2.612e+04 more rows
```

It can be noticed that the common *dplyr* verbs, such as `summarise()`, `mutate()`, `select()`, `filter()`, and `arrange()`, work with the tsibble.

### `fill_na()` to turn implicit missing values into explicit missing values

Often there are implicit missing cases in temporal data. If the observations are made at regular time interval, we'd like to turn these implicit missings to be explicit. The `fill_na()` function not only extends the index and key to make the `NA`s present, but also provides a consistent interface to replace these `NA`s using a set of name-value pairs.

``` r
full_pedestrian <- pedestrian %>%
  fill_na(
    Date = lubridate::as_date(Date_Time),
    Time = lubridate::hour(Date_Time)
  )
c(nrow(pedestrian), nrow(full_pedestrian))
#> [1] 66071 70176
full_pedestrian
#> # A tsibble: 70,176 x 5 [1HOUR]
#> # Keys:      Sensor
#>                          Sensor           Date_Time       Date  Time Count
#> *                         <chr>              <dttm>     <date> <int> <int>
#> 1                Birrarung Marr 2015-01-01 00:00:00 2015-01-01     0  1630
#> 2    Bourke Street Mall (North) 2015-01-01 00:00:00 2015-01-01     0    NA
#> 3 QV Market-Elizabeth St (West) 2015-01-01 00:00:00 2015-01-01     0   490
#> 4        Southern Cross Station 2015-01-01 00:00:00 2015-01-01     0   746
#> 5                Birrarung Marr 2015-01-01 01:00:00 2015-01-01     1   826
#> # ... with 7.017e+04 more rows
```

In the example of `pedestrian`, the missing values of the *Date* and *Time*, are supplied by the corresponding component of the `Date_Time`. The rest of untouched variables (i.e. `Count`) simply leave NA as is.

Related work
------------

-   [tibbletime](https://github.com/business-science/tibbletime): time-aware tibbles. We have different APIs and thinking about temporal data.
-   [padr](https://github.com/EdwinTh/padr): padding of missing records in time series. We do more.
