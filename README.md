
<!-- README.md is generated from README.Rmd. Please edit that file -->
tsibble
=======

[![Travis-CI Build Status](https://travis-ci.org/earowang/tsibble.svg?branch=master)](https://travis-ci.org/earowang/tsibble) [![Coverage Status](https://img.shields.io/codecov/c/github/earowang/tsibble/master.svg)](https://codecov.io/github/earowang/tsibble?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tsibble)](https://cran.r-project.org/package=tsibble)

Overview
--------

The **tsibble** package provides a data class of `tbl_ts` to manage temporal data frames in a tidy and modern way. A *tsibble* consists of a time index, keys and other measured variables in a data-centric format, which is built on top of the *tibble*.

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
#>    origin  year month   day  hour  temp  dewp humid wind_dir wind_speed
#>  *  <chr> <dbl> <dbl> <int> <int> <dbl> <dbl> <dbl>    <dbl>      <dbl>
#>  1    EWR  2013     1     1     0 37.04 21.92 53.97      230   10.35702
#>  2    EWR  2013     1     1     1 37.04 21.92 53.97      230   13.80936
#>  3    EWR  2013     1     1     2 37.94 21.92 52.09      230   12.65858
#>  4    EWR  2013     1     1     3 37.94 23.00 54.51      230   13.80936
#>  5    EWR  2013     1     1     4 37.94 24.08 57.04      240   14.96014
#>  6    EWR  2013     1     1     6 39.02 26.06 59.37      270   10.35702
#>  7    EWR  2013     1     1     7 39.02 26.96 61.63      250    8.05546
#>  8    EWR  2013     1     1     8 39.02 28.04 64.43      240   11.50780
#>  9    EWR  2013     1     1     9 39.92 28.04 62.21      250   12.65858
#> 10    EWR  2013     1     1    10 39.02 28.04 64.43      260   12.65858
#> # ... with 26,120 more rows, and 5 more variables: wind_gust <dbl>,
#> #   precip <dbl>, pressure <dbl>, visib <dbl>, time_hour <dttm>
```

The **key** is not constrained to a single variable, but expressive for nested and crossed data structures. See `vignette()` for details.

### Summarise over calendar periods with `tsummarise()`

The common *dplyr* verbs, such as `summarise()`, `mutate()`, `select()`, `filter()`, and `arrange()`, work with the tsibble. We also have a new verb `tsummarise()` to aggregate interested variables over calendar periods.

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
#>    origin year_month avg_temp ttl_precip
#>  *  <chr>      <mth>    <dbl>      <dbl>
#>  1    EWR   2013 Jan 35.45787       2.70
#>  2    EWR   2013 Feb 34.09193       2.76
#>  3    EWR   2013 Mar 39.98992       1.92
#>  4    EWR   2013 Apr 52.75792       1.07
#>  5    EWR   2013 May 62.75508       2.76
#>  6    EWR   2013 Jun 73.38200       5.12
#>  7    EWR   2013 Jul 80.65563       1.66
#>  8    EWR   2013 Aug 74.40011       2.65
#>  9    EWR   2013 Sep 67.52960       0.94
#> 10    EWR   2013 Oct 59.77984       0.14
#> # ... with 26 more rows
```

The `tsummarise` goes hand in hand with the index functions including `as.Date()`, `yearmth()`, `yearqtr()`, and `year()`.

Related work
------------

-   [tibbletime](https://github.com/business-science/tibbletime)
-   [tsbox](https://github.com/christophsax/tsbox)
