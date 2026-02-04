# Tidyverse methods for tsibble

Current dplyr verbs that tsibble has support for:

- [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html),
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

- [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
  [`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html),
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  [`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`dplyr::right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
  [`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
  [`dplyr::nest_join()`](https://dplyr.tidyverse.org/reference/nest_join.html)

- [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html),
  [`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)

Current tidyr verbs that tsibble has support for:

- [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html),
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html),
  [`tidyr::gather()`](https://tidyr.tidyverse.org/reference/gather.html),
  [`tidyr::spread()`](https://tidyr.tidyverse.org/reference/spread.html)

- [`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html),
  [`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html),
  [`tidyr::drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)

## Column-wise verbs

- The index variable cannot be dropped for a tsibble object.

- When any key variable is modified, a check on the validity of the
  resulting tsibble will be performed internally.

- Use
  [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  to convert tsibble to a general data frame.

## Row-wise verbs

A warning is likely to be issued, if observations are not arranged in
past-to-future order.

## Join verbs

Joining with other data sources triggers the check on the validity of
the resulting tsibble.

## Examples

``` r
library(dplyr, warn.conflicts = FALSE)
# `summarise()` a tsibble always aggregates over time
# Sum over sensors
pedestrian %>%
  index_by() %>%
  summarise(Total = sum(Count))
#> # A tsibble: 17,542 x 2 [1h] <Australia/Melbourne>
#>    Date_Time           Total
#>    <dttm>              <int>
#>  1 2015-01-01 00:00:00  2866
#>  2 2015-01-01 01:00:00  1535
#>  3 2015-01-01 02:00:00   994
#>  4 2015-01-01 03:00:00   569
#>  5 2015-01-01 04:00:00   311
#>  6 2015-01-01 05:00:00   159
#>  7 2015-01-01 06:00:00   129
#>  8 2015-01-01 07:00:00   146
#>  9 2015-01-01 08:00:00   258
#> 10 2015-01-01 09:00:00   419
#> # ℹ 17,532 more rows
# shortcut
pedestrian %>%
  summarise(Total = sum(Count))
#> # A tsibble: 17,542 x 2 [1h] <Australia/Melbourne>
#>    Date_Time           Total
#>    <dttm>              <int>
#>  1 2015-01-01 00:00:00  2866
#>  2 2015-01-01 01:00:00  1535
#>  3 2015-01-01 02:00:00   994
#>  4 2015-01-01 03:00:00   569
#>  5 2015-01-01 04:00:00   311
#>  6 2015-01-01 05:00:00   159
#>  7 2015-01-01 06:00:00   129
#>  8 2015-01-01 07:00:00   146
#>  9 2015-01-01 08:00:00   258
#> 10 2015-01-01 09:00:00   419
#> # ℹ 17,532 more rows
# Back to tibble
pedestrian %>%
  as_tibble() %>%
  summarise(Total = sum(Count))
#> # A tibble: 1 × 1
#>      Total
#>      <int>
#> 1 45483871

library(tidyr)
stocks <- tsibble(
  time = as.Date("2009-01-01") + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
#> Using `time` as index variable.
(stocksm <- stocks %>%
  pivot_longer(-time, names_to = "stock", values_to = "price"))
#> # A tsibble: 30 x 3 [1D]
#> # Key:       stock [3]
#>    time       stock   price
#>    <date>     <chr>   <dbl>
#>  1 2009-01-01 X      1.04  
#>  2 2009-01-01 Y     -1.73  
#>  3 2009-01-01 Z      3.62  
#>  4 2009-01-02 X     -0.450 
#>  5 2009-01-02 Y      4.87  
#>  6 2009-01-02 Z      4.81  
#>  7 2009-01-03 X     -0.709 
#>  8 2009-01-03 Y      0.0877
#>  9 2009-01-03 Z      4.72  
#> 10 2009-01-04 X      0.0304
#> # ℹ 20 more rows
stocksm %>%
  pivot_wider(names_from = stock, values_from = price)
#> # A tsibble: 10 x 4 [1D]
#>    time             X       Y       Z
#>    <date>       <dbl>   <dbl>   <dbl>
#>  1 2009-01-01  1.04   -1.73    3.62  
#>  2 2009-01-02 -0.450   4.87    4.81  
#>  3 2009-01-03 -0.709   0.0877  4.72  
#>  4 2009-01-04  0.0304 -0.637  -4.27  
#>  5 2009-01-05 -0.450   5.38    1.30  
#>  6 2009-01-06  0.671   1.14   -0.770 
#>  7 2009-01-07  1.77   -1.09    2.94  
#>  8 2009-01-08  0.635  -0.467   5.09  
#>  9 2009-01-09  0.368  -2.93   -1.02  
#> 10 2009-01-10  1.38    0.784  -0.0402
```
