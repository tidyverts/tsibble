
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsibble <img src="man/figures/logo.png" align="right" />

[![Travis-CI Build
Status](https://travis-ci.org/tidyverts/tsibble.svg?branch=master)](https://travis-ci.org/tidyverts/tsibble)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/tidyverts/tsibble?branch=master&svg=true)](https://ci.appveyor.com/project/tidyverts/tsibble)
[![Coverage
Status](https://codecov.io/gh/tidyverts/tsibble/branch/master/graph/badge.svg)](https://codecov.io/github/tidyverts/tsibble?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tsibble)](https://cran.r-project.org/package=tsibble)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

The **tsibble** package provides a data infrastructure for tidy temporal
data with wrangling tools. Adapting the [tidy data
principles](https://tidyr.tidyverse.org/articles/tidy-data.html),
*tsibble* is an explicit data- and model-oriented object. In *tsibble*:

1.  Index is a variable with inherent ordering from past to present.
2.  Key is a set of variables that define observational units over time.
3.  Each observation should be uniquely identified by **index** and
    **key**.
4.  Each observational unit should be measured at a common **interval**,
    if regularly spaced.

## Installation

You could install the stable version on CRAN:

``` r
install.packages("tsibble")
```

You could install the development version from Github using

``` r
# install.packages("remotes")
remotes::install_github("tidyverts/tsibble")
```

## Get started

### Coerce to a tsibble with `as_tsibble()`

To coerce a data frame to *tsibble*, we need to declare key and index.
For example, in the `weather` data from the package `nycflights13`, the
`time_hour` containing the date-times should be declared as **index**,
and the `origin` as **key**. Other columns can be considered as measured
variables.

``` r
library(dplyr)
library(tsibble)
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather_tsbl <- as_tsibble(weather, key = origin, index = time_hour)
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
#> # … with 2.611e+04 more rows
```

The **key** can be comprised of empty, one, or more variables. See
`package?tsibble` and
[`vignette("intro-tsibble")`](http://tsibble.tidyverts.org/articles/intro-tsibble.html)
for details.

The interval is obtained based on index representation, ranging from
year to nanosecond, from numerics to ordered factors. The table below
shows how tsibble interprets the common time formats.

| **Interval** | **Class**                 |
| ------------ | ------------------------- |
| Annual       | `integer`/`double`        |
| Quarterly    | `yearquarter`             |
| Monthly      | `yearmonth`               |
| Weekly       | `yearweek`                |
| Daily        | `Date`/`difftime`         |
| Subdaily     | `POSIXt`/`difftime`/`hms` |

### `fill_gaps()` to turn implicit missing values into explicit missing values

Often there are implicit missing cases in time series. If the
observations are made at regular time interval, we could turn these
implicit missingness to be explicit simply using `fill_gaps()`, filling
gaps in precipitation (`precip`) with 0 in the meanwhile. It is quite
common to replaces `NA`s with its previous observation for each origin
in time series analysis, which is easily done using `fill()` from
**tidyr**.

``` r
full_weather <- weather_tsbl %>%
  fill_gaps(precip = 0) %>% 
  group_by_key() %>% 
  tidyr::fill(temp, humid, .direction = "down")
full_weather
#> # A tsibble: 26,190 x 5 [1h] <America/New_York>
#> # Key:       origin [3]
#> # Groups:    origin [3]
#>   origin time_hour            temp humid precip
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>
#> 1 EWR    2013-01-01 01:00:00  39.0  59.4      0
#> 2 EWR    2013-01-01 02:00:00  39.0  61.6      0
#> 3 EWR    2013-01-01 03:00:00  39.0  64.4      0
#> 4 EWR    2013-01-01 04:00:00  39.9  62.2      0
#> 5 EWR    2013-01-01 05:00:00  39.0  64.4      0
#> # … with 2.618e+04 more rows
```

`fill_gaps()` also handles filling in time gaps by values or functions,
and respects time zones for date-times. Wanna a quick overview of
implicit missing values? Check out
[`vignette("implicit-na")`](http://tsibble.tidyverts.org/articles/implicit-na.html).

### `index_by()` + `summarise()` to aggregate over calendar periods

`index_by()` is the counterpart of `group_by()` in temporal context, but
it groups the index only. In conjunction with `index_by()`,
`summarise()` and its scoped variants aggregate interested variables
over calendar periods. `index_by()` goes hand in hand with the index
functions including `as.Date()`, `yearweek()`, `yearmonth()`, and
`yearquarter()`, as well as other friends from **lubridate**. For
example, it would be of interest in computing average temperature and
total precipitation per month, by applying `yearmonth()` to the index
variable (referred to as `.`).

``` r
full_weather %>%
  group_by_key() %>%
  index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(
    avg_temp = mean(temp, na.rm = TRUE),
    ttl_precip = sum(precip, na.rm = TRUE)
  )
#> # A tsibble: 36 x 4 [1M]
#> # Key:       origin [3]
#>   origin year_month avg_temp ttl_precip
#>   <chr>       <mth>    <dbl>      <dbl>
#> 1 EWR      2013 Jan     35.6       3.53
#> 2 EWR      2013 Feb     34.2       3.83
#> 3 EWR      2013 Mar     40.1       3   
#> 4 EWR      2013 Apr     53.0       1.47
#> 5 EWR      2013 May     63.3       5.44
#> # … with 31 more rows
```

While collapsing rows (like `summarise()`), `group_by()` and
`index_by()` will take care of updating the key and index respectively.
This `index_by()` + `summarise()` combo can help with regularising a
tsibble of irregular time space
too.

### Rolling with functional programming: `slide()`, `tile()`, `stretch()`

Temporal data often involves moving window calculations. Several
functions in **tsibble** allow for different variations of moving
windows using purrr-like syntax:

  - `slide()`/`slide2()`/`pslide()`: sliding window with overlapping
    observations.
  - `tile()`/`tile2()`/`ptile()`: tiling window without overlapping
    observations.
  - `stretch()`/`stretch2()`/`pstretch()`: fixing an initial window and
    expanding to include more observations.

<details>

<summary>Rolling window animation</summary>

<img src="man/figures/animate-1.gif" align="center" />

</details>

For example, a moving average of window size 3 is carried out on hourly
temperatures for each group (*origin*).

``` r
full_weather %>% 
  group_by_key() %>% 
  mutate(temp_ma = slide_dbl(temp, ~ mean(., na.rm = TRUE), .size = 3))
#> # A tsibble: 26,190 x 6 [1h] <America/New_York>
#> # Key:       origin [3]
#> # Groups:    origin [3]
#>   origin time_hour            temp humid precip temp_ma
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>   <dbl>
#> 1 EWR    2013-01-01 01:00:00  39.0  59.4      0    NA  
#> 2 EWR    2013-01-01 02:00:00  39.0  61.6      0    NA  
#> 3 EWR    2013-01-01 03:00:00  39.0  64.4      0    39.0
#> 4 EWR    2013-01-01 04:00:00  39.9  62.2      0    39.3
#> 5 EWR    2013-01-01 05:00:00  39.0  64.4      0    39.3
#> # … with 2.618e+04 more rows
```

Looking for rolling in parallel? Their multiprocessing equivalents are
prefixed with `future_`. More examples can be found at
[`vignette("window")`](https://tsibble.tidyverts.org/articles/window.html).

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/tidyverts/tsibble/blob/master/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
