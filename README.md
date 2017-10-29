
<!-- README.md is generated from README.Rmd. Please edit that file -->
tsibble
=======

[![Travis-CI Build Status](https://travis-ci.org/earowang/tsibble.svg?branch=master)](https://travis-ci.org/earowang/tsibble) [![Coverage Status](https://img.shields.io/codecov/c/github/earowang/tsibble/master.svg)](https://codecov.io/github/earowang/tsibble?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tsibble)](https://cran.r-project.org/package=tsibble)

Overview
--------

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
weather_ts <- as_tsibble(nycflights13::weather, origin, index = time_hour)
weather_ts
#> # A tsibble: 26,130 x 15 [1HOUR]
#> # Keys:      origin
#>   origin  year month   day  hour  temp  dewp humid wind_dir wind_speed
#> *  <chr> <dbl> <dbl> <int> <int> <dbl> <dbl> <dbl>    <dbl>      <dbl>
#> 1    EWR  2013     1     1     0 37.04 21.92 53.97      230   10.35702
#> 2    EWR  2013     1     1     1 37.04 21.92 53.97      230   13.80936
#> 3    EWR  2013     1     1     2 37.94 21.92 52.09      230   12.65858
#> 4    EWR  2013     1     1     3 37.94 23.00 54.51      230   13.80936
#> 5    EWR  2013     1     1     4 37.94 24.08 57.04      240   14.96014
#> # ... with 2.612e+04 more rows, and 5 more variables: wind_gust <dbl>,
#> #   precip <dbl>, pressure <dbl>, visib <dbl>, time_hour <dttm>
```

The **key** is not constrained to a single variable, but expressive for nested and crossed data structures. See `?tsibble` and `vignette()` for details.

### tsibble verbs

The common *dplyr* verbs, such as `summarise()`, `mutate()`, `select()`, `filter()`, and `arrange()`, work with the tsibble.

-   `tsummarise()` to summarise over calendar periods

We have a new verb `tsummarise()` to aggregate interested variables over calendar periods.

``` r
weather_ts %>%
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

The `tsummarise` goes hand in hand with the index functions including `as.Date()`, `yearmth()`, `yearqtr()`, `year()`, and other friends from *lubridate*, like `ceiling_date()`.

-   `fill_na()` to turn implicit missing values into explicit missing values

Often, there are implicit missing cases in temporal data. If the observations are made at regular time interval, we'd like to turn these implicit missings to be explicit. The `fill_na()` function not only makes the `NA`s present, but also provides a consistent interface to replace these `NA`s.

``` r
nr <- nrow(weather_ts)
# randomly remove 20% of the observations
weather_na <- weather_ts %>%
  slice(sample(nr, size = nr * 0.8))
# replace NA with either functions or values for each group
weather_na %>%
  group_by(origin) %>%
  fill_na(
    year = as.numeric(year(time_hour)),
    temp = mean(temp, na.rm = TRUE),
    precip = 0
  )
#> # A tsibble: 26,208 x 15 [1HOUR]
#> # Keys:      origin
#>   origin  year month   day  hour     temp  dewp humid wind_dir wind_speed
#> *  <chr> <dbl> <dbl> <int> <int>    <dbl> <dbl> <dbl>    <dbl>      <dbl>
#> 1    EWR  2013     1     1     0 37.04000 21.92 53.97      230   10.35702
#> 2    EWR  2013     1     1     1 37.04000 21.92 53.97      230   13.80936
#> 3    EWR  2013    NA    NA    NA 55.40617    NA    NA       NA         NA
#> 4    EWR  2013     1     1     3 37.94000 23.00 54.51      230   13.80936
#> 5    EWR  2013     1     1     4 37.94000 24.08 57.04      240   14.96014
#> # ... with 2.62e+04 more rows, and 5 more variables: wind_gust <dbl>,
#> #   precip <dbl>, pressure <dbl>, visib <dbl>, time_hour <dttm>
```

If there's no replacement value for some variables, leave `NA` as is.

Related work
------------

-   [tibbletime](https://github.com/business-science/tibbletime)
-   [tsbox](https://github.com/christophsax/tsbox)
