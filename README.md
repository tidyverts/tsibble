
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsibble

*/ˈt͡sɪbəl/*

[![Travis-CI Build
Status](https://travis-ci.org/earowang/tsibble.svg?branch=master)](https://travis-ci.org/earowang/tsibble)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/earowang/tsibble?branch=master&svg=true)](https://ci.appveyor.com/project/earowang/tsibble)
[![Coverage
Status](https://img.shields.io/codecov/c/github/earowang/tsibble/master.svg)](https://codecov.io/github/earowang/tsibble?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tsibble)](https://cran.r-project.org/package=tsibble)
[![Downloads](http://cranlogs.r-pkg.org/badges/tsibble?color=brightgreen)](https://cran.r-project.org/package=tsibble)

The **tsibble** package provides a data class of `tbl_ts` to store and
manage temporal-context data frames in a tidy manner. A *tsibble*
consists of a time index, keys and other measured variables in a
data-centric format, which is built on top of the *tibble*.

## Installation

You could install the stable version on CRAN:

``` r
install.packages("tsibble")
```

You could install the development version from Github using

``` r
# install.packages("devtools")
devtools::install_github("earowang/tsibble", build_vignettes = TRUE)
```

## Get started

### Coerce to a tsibble with `as_tsibble()`

The `weather` data included in the package `nycflights13` is used as an
example to illustrate. The “index” variable is the `time_hour`
containing the date-times, and the “key” is the `origin` as weather
stations created via the `id()`. **The key(s) together with the index
uniquely identifies each observation**, which gives a valid *tsibble*.
Other columns can be considered as measured variables.

``` r
library(tsibble)
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather_tsbl <- as_tsibble(weather, key = id(origin), index = time_hour)
weather_tsbl
#> # A tsibble: 26,130 x 5 [1HOUR]
#> # Keys:      origin [3]
#>   origin time_hour            temp humid precip
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>
#> 1 EWR    2013-01-01 00:00:00  37.0  54.0      0
#> 2 EWR    2013-01-01 01:00:00  37.0  54.0      0
#> 3 EWR    2013-01-01 02:00:00  37.9  52.1      0
#> 4 EWR    2013-01-01 03:00:00  37.9  54.5      0
#> 5 EWR    2013-01-01 04:00:00  37.9  57.0      0
#> # ... with 2.612e+04 more rows
```

The **key** is not constrained to a single variable, but expressive of
nested and crossed data structures. This incorporates univariate,
multivariate, hierarchical and grouped time series into the tsibble
framework. See `?tsibble` and
[`vignette("intro-tsibble")`](http://pkg.earo.me/tsibble/articles/intro-tsibble.html)
for
details.

### `fill_na()` to turn implicit missing values into explicit missing values

Often there are implicit missing cases in temporal data. If the
observations are made at regular time interval, we could turn these
implicit missings to be explicit simply using `fill_na()`. Meanwhile,
fill `NA`s in by 0 for precipitation (`precip`). It is quite common to
replaces `NA`s with its previous observation for each origin in time
series analysis, which is easily done using `fill()` from *tidyr*.

``` r
full_weather <- weather_tsbl %>%
  fill_na(precip = 0) %>% 
  group_by(origin) %>% 
  fill(temp, humid, .direction = "down")
full_weather
#> # A tsibble: 26,208 x 5 [1HOUR]
#> # Keys:      origin [3]
#> # Groups:    origin [3]
#>   origin time_hour            temp humid precip
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>
#> 1 EWR    2013-01-01 00:00:00  37.0  54.0      0
#> 2 EWR    2013-01-01 01:00:00  37.0  54.0      0
#> 3 EWR    2013-01-01 02:00:00  37.9  52.1      0
#> 4 EWR    2013-01-01 03:00:00  37.9  54.5      0
#> 5 EWR    2013-01-01 04:00:00  37.9  57.0      0
#> # ... with 2.62e+04 more rows
```

`fill_na()` also handles filling `NA` by values or functions, and
preserves time zones for date-times.

### `tsummarise()` to summarise over calendar periods

`tsummarise()` is introduced to aggregate interested variables over
calendar periods. The `tsummarise` goes hand in hand with the index
functions including `as.Date()`, `yearmonth()`, and `yearquarter()`, as
well as other friends from *lubridate*, such as `year()` and
`ceiling_date()`. For example, it would be of interest in computing
average temperature and total precipitation per month, by applying the
`yearmonth()` to the hourly time index.

``` r
full_weather %>%
  group_by(origin) %>%
  tsummarise(
    year_month = yearmonth(time_hour), # monthly aggregates
    avg_temp = mean(temp, na.rm = TRUE),
    ttl_precip = sum(precip, na.rm = TRUE)
  )
#> # A tsibble: 36 x 4 [1MONTH]
#> # Keys:      origin [3]
#>   origin year_month avg_temp ttl_precip
#>   <chr>       <mth>    <dbl>      <dbl>
#> 1 EWR      2013 Jan     35.6       2.70
#> 2 EWR      2013 Feb     34.1       2.76
#> 3 EWR      2013 Mar     40.0       1.94
#> 4 EWR      2013 Apr     52.9       1.05
#> 5 EWR      2013 May     63.1       2.76
#> # ... with 31 more rows
```

The `tsummarise()` can also help with regularising a tsibble of
irregular time space.

### A family of window functions: `slide()`, `tile()`, `stretch()`

Temporal data often involves moving window calculations. Several
functions in the *tsibble* allow for different variations of moving
windows using purrr-like syntax:

  - `slide()`: sliding window with overlapping observations.
  - `tile()`: tiling window without overlapping observations.
  - `stretch()`: fixing an initial window and expanding more
    observations.

For example, a moving average of window size 3 is carried out on hourly
temperatures for each group (*origin*).

``` r
full_weather %>% 
  group_by(origin) %>% 
  mutate(temp_ma = slide(temp, ~ mean(., na.rm = TRUE), size = 3))
#> # A tsibble: 26,208 x 6 [1HOUR]
#> # Keys:      origin [3]
#> # Groups:    origin [3]
#>   origin time_hour            temp humid precip temp_ma
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>   <dbl>
#> 1 EWR    2013-01-01 00:00:00  37.0  54.0      0    NA  
#> 2 EWR    2013-01-01 01:00:00  37.0  54.0      0    NA  
#> 3 EWR    2013-01-01 02:00:00  37.9  52.1      0    37.3
#> 4 EWR    2013-01-01 03:00:00  37.9  54.5      0    37.6
#> 5 EWR    2013-01-01 04:00:00  37.9  57.0      0    37.9
#> # ... with 2.62e+04 more rows
```

### Reexported functions from the tidyverse

It can be noticed that the tsibble seamlessly works with *dplyr* verbs.

  - **dplyr:**
      - `arrange()`, `filter()`, `slice()`
      - `mutate()`, `select()`, `summarise()`/`summarize()` with an
        additional argument `drop = FALSE` to drop `tbl_ts` and coerce
        to `tbl_df`
      - `rename()`
      - `*_join()`
      - `group_by()`, `ungroup()`
      - 🚫 `transmute()`, `disintct()`
  - **tidyr:** `fill()`
  - **tibble:** `glimpse()`, `as_tibble()`/`as.tibble()`
  - **rlang:** `!!`, `!!!`

## Related work

  - [zoo](https://CRAN.R-project.org/package=zoo): regular and irregular
    time series with methods.
  - [xts](https://github.com/joshuaulrich/xts): extensible time series.
  - [tibbletime](https://github.com/business-science/tibbletime):
    time-aware tibbles.
  - [padr](https://github.com/EdwinTh/padr): padding of missing records
    in time series.
