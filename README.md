
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsibble <img src="man/figures/logo.png" align="right" />

*/ˈt͡sɪbəl/*

[![Travis-CI Build
Status](https://travis-ci.org/tidyverts/tsibble.svg?branch=master)](https://travis-ci.org/tidyverts/tsibble)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tidyverts/tsibble/master.svg)](https://codecov.io/github/tidyverts/tsibble?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tsibble)](https://cran.r-project.org/package=tsibble)
[![Downloads](http://cranlogs.r-pkg.org/badges/tsibble?color=brightgreen)](https://cran.r-project.org/package=tsibble)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

The **tsibble** package provides a data class of `tbl_ts` to represent
tidy temporal-context data. A *tsibble* consists of a time index, key
and other measured variables in a data-centric format, which is built on
top of the *tibble*.

## Installation

You could install the stable version on CRAN:

``` r
install.packages("tsibble")
```

You could install the development version from Github using

``` r
# install.packages("devtools")
devtools::install_github("tidyverts/tsibble", build_vignettes = TRUE)
```

## Usage

### Coerce to a tsibble with `as_tsibble()`

The `weather` data included in the package `nycflights13` is used as an
example to illustrate. The “index” variable is the `time_hour`
containing the date-times, and the “key” is the `origin` as weather
stations created via `id()`. **The key together with the index uniquely
identifies each observation**, which gives a valid *tsibble*. Other
columns can be considered as measured variables.

``` r
library(tsibble)
weather <- nycflights13::weather %>% 
  select(origin, time_hour, temp, humid, precip)
weather_tsbl <- as_tsibble(weather, key = id(origin), index = time_hour)
weather_tsbl
#> # A tsibble: 26,115 x 5 [1h]
#> # Key:       origin [3]
#>   origin time_hour            temp humid precip
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>
#> 1 EWR    2013-01-01 01:00:00  39.0  59.4      0
#> 2 EWR    2013-01-01 02:00:00  39.0  61.6      0
#> 3 EWR    2013-01-01 03:00:00  39.0  64.4      0
#> 4 EWR    2013-01-01 04:00:00  39.9  62.2      0
#> 5 EWR    2013-01-01 05:00:00  39.0  64.4      0
#> # ... with 2.611e+04 more rows
```

The **key** is not constrained to a single variable, but expressive of
nested and crossed data structures. This incorporates univariate,
multivariate, hierarchical and grouped time series into the *tsibble*
framework. See `package?tsibble` and
[`vignette("intro-tsibble")`](http://pkg.earo.me/tsibble/articles/intro-tsibble.html)
for details.

The *tsibble* internally computes the interval for given time indices
based on the time representation, ranging from year to nanosecond. The
`POSIXct` corresponds to sub-daily series, `Date` to daily, `yearweek`
to weekly, `yearmonth`/`yearmth` to monthly, `yearquarter`/`yearqtr` to
quarterly, and
etc.

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
  tidyr::fill(temp, humid, .direction = "down")
full_weather
#> # A tsibble: 26,190 x 5 [1h]
#> # Key:       origin [3]
#> # Groups:    origin [3]
#>   origin time_hour            temp humid precip
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>
#> 1 EWR    2013-01-01 01:00:00  39.0  59.4      0
#> 2 EWR    2013-01-01 02:00:00  39.0  61.6      0
#> 3 EWR    2013-01-01 03:00:00  39.0  64.4      0
#> 4 EWR    2013-01-01 04:00:00  39.9  62.2      0
#> 5 EWR    2013-01-01 05:00:00  39.0  64.4      0
#> # ... with 2.618e+04 more rows
```

`fill_na()` also handles filling `NA` by values or functions, and
preserves time zones for date-times. Wanna a quick overview of implicit
time gaps? Check out `count_gaps()`.

### `index_by()` + `summarise()` to aggregate over calendar periods

`index_by()` is the counterpart of `group_by()` in temporal context, but
it groups the index only. In conjunction with `index_by()`,
`summarise()` and its scoped variants aggregate interested variables
over calendar periods. `index_by()` goes hand in hand with the index
functions including `as.Date()`, `yearweek()`, `yearmonth()`, and
`yearquarter()`, as well as other friends from *lubridate*. For example,
it would be of interest in computing average temperature and total
precipitation per month, by applying `yearmonth()` to the hourly time
index.

``` r
full_weather %>%
  group_by(origin) %>%
  index_by(year_month = yearmonth(time_hour)) %>% # monthly aggregates
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
#> # ... with 31 more rows
```

While collapsing rows (like `summarise()`), `group_by()` and
`index_by()` will take care of updating the key and index respectively.
This `index_by()` + `summarise()` combo can help with regularising a
tsibble of irregular time space too.

### A family of window functions: `slide()`, `tile()`, `stretch()`

Temporal data often involves moving window calculations. Several
functions in *tsibble* allow for different variations of moving windows
using purrr-like syntax:

  - `slide()`/`slide2()`/`pslide()`: sliding window with overlapping
    observations.
  - `tile()`/`tile2()`/`ptile()`: tiling window without overlapping
    observations.
  - `stretch()`/`stretch2()`/`pstretch()`: fixing an initial window and
    expanding to include more observations.

For example, a moving average of window size 3 is carried out on hourly
temperatures for each group (*origin*).

``` r
full_weather %>% 
  group_by(origin) %>% 
  mutate(temp_ma = slide_dbl(temp, ~ mean(., na.rm = TRUE), .size = 3))
#> # A tsibble: 26,190 x 6 [1h]
#> # Key:       origin [3]
#> # Groups:    origin [3]
#>   origin time_hour            temp humid precip temp_ma
#>   <chr>  <dttm>              <dbl> <dbl>  <dbl>   <dbl>
#> 1 EWR    2013-01-01 01:00:00  39.0  59.4      0    NA  
#> 2 EWR    2013-01-01 02:00:00  39.0  61.6      0    NA  
#> 3 EWR    2013-01-01 03:00:00  39.0  64.4      0    39.0
#> 4 EWR    2013-01-01 04:00:00  39.9  62.2      0    39.3
#> 5 EWR    2013-01-01 05:00:00  39.0  64.4      0    39.3
#> # ... with 2.618e+04 more rows
```

More examples can be found at
[`vignette("window")`](https://pkg.earo.me/tsibble/articles/window.html).

## Working with the tidyverse

It can be noticed that the tsibble seamlessly works with *tidyverse*
verbs, but in a slightly different way that it does the best to keep the
index. Use `?tidyverse` for a full list of tidyverse functions.

  - **dplyr:**
      - `arrange()`, `filter()`, `slice()`
      - `mutate()`, `transmute()`, `select()`, `rename()`,
        `summarise()`/`summarize()`
      - `left/right/full/inner/anti/semi_join()`
      - `group_by()`, `ungroup()`
  - **tidyr**: `gather()`, `spread()`, `nest()`, `unnest()`
  - **tibble:** `glimpse()`, `tibble()`, `as_tibble()`
  - **rlang:** `!!`, `!!!`

-----

Please note that this project is released with a [Contributor Code of
Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.
