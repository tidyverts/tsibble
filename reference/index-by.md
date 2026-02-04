# Group by time index and collapse with `summarise()`

**\[stable\]**

`index_by()` is the counterpart of
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) in
temporal context, but it only groups the time index. The following
operation is applied to each partition of the index, similar to
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) but
dealing with index only. `index_by()` +
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
will update the grouping index variable to be the new index. Use
[`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html) to
remove the index grouping vars.

## Usage

``` r
index_by(.data, ...)
```

## Arguments

- .data:

  A `tbl_ts`.

- ...:

  If empty, grouping the current index. If not empty, a single
  expression is required for either an existing variable or a name-value
  pair. A lambda expression is supported, for example `~ as.Date(.)`
  where `.` refers to the index variable. The index functions that can
  be used, but not limited:

  - [lubridate::year](https://lubridate.tidyverse.org/reference/year.html):
    yearly aggregation

  - [yearquarter](https://tsibble.tidyverts.org/reference/year-quarter.md):
    quarterly aggregation

  - [yearmonth](https://tsibble.tidyverts.org/reference/year-month.md):
    monthly aggregation

  - [yearweek](https://tsibble.tidyverts.org/reference/year-week.md):
    weekly aggregation

  - [as.Date](https://rdrr.io/r/base/as.Date.html) or
    [lubridate::as_date](https://lubridate.tidyverse.org/reference/as_date.html):
    daily aggregation

  - [lubridate::ceiling_date](https://lubridate.tidyverse.org/reference/round_date.html),
    [lubridate::floor_date](https://lubridate.tidyverse.org/reference/round_date.html),
    or
    [lubridate::round_date](https://lubridate.tidyverse.org/reference/round_date.html):
    fine-resolution aggregation

  - Extract time components functions, such as
    [`lubridate::hour()`](https://lubridate.tidyverse.org/reference/hour.html)
    &
    [`lubridate::day()`](https://lubridate.tidyverse.org/reference/day.html)

  - other index functions from other packages or self-defined functions

## Details

- A `index_by()`-ed tsibble is indicated by `@` in the "Groups" when
  displaying on the screen.

## Examples

``` r
pedestrian %>% index_by()
#> # A tsibble: 66,037 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#> # Groups:    @ Date_Time [17,542]
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
#> # ℹ 66,027 more rows
# Monthly counts across sensors
library(dplyr, warn.conflicts = FALSE)
monthly_ped <- pedestrian %>%
  group_by_key() %>%
  index_by(Year_Month = ~ yearmonth(.)) %>%
  summarise(
    Max_Count = max(Count),
    Min_Count = min(Count)
  )
monthly_ped
#> # A tsibble: 95 x 4 [1M]
#> # Key:       Sensor [4]
#>    Sensor         Year_Month Max_Count Min_Count
#>    <chr>               <mth>     <int>     <int>
#>  1 Birrarung Marr   2015 Jan      5524         1
#>  2 Birrarung Marr   2015 Feb     10121         1
#>  3 Birrarung Marr   2015 Mar      9858         1
#>  4 Birrarung Marr   2015 Apr      7293         1
#>  5 Birrarung Marr   2015 May      5129         1
#>  6 Birrarung Marr   2015 Jun      7556         0
#>  7 Birrarung Marr   2015 Jul     11224         1
#>  8 Birrarung Marr   2015 Aug      5684         0
#>  9 Birrarung Marr   2015 Sep      7757         0
#> 10 Birrarung Marr   2015 Oct      7085         1
#> # ℹ 85 more rows
index(monthly_ped)
#> Year_Month

# Using existing variable
pedestrian %>%
  group_by_key() %>%
  index_by(Date) %>%
  summarise(
    Max_Count = max(Count),
    Min_Count = min(Count)
  )
#> # A tsibble: 2,752 x 4 [1D]
#> # Key:       Sensor [4]
#>    Sensor         Date       Max_Count Min_Count
#>    <chr>          <date>         <int>     <int>
#>  1 Birrarung Marr 2015-01-01      1630        44
#>  2 Birrarung Marr 2015-01-02       352         1
#>  3 Birrarung Marr 2015-01-03       226         3
#>  4 Birrarung Marr 2015-01-04       852         4
#>  5 Birrarung Marr 2015-01-05      1427         3
#>  6 Birrarung Marr 2015-01-06       937         5
#>  7 Birrarung Marr 2015-01-07       708         4
#>  8 Birrarung Marr 2015-01-08       568         9
#>  9 Birrarung Marr 2015-01-09      1629         5
#> 10 Birrarung Marr 2015-01-10      2439        10
#> # ℹ 2,742 more rows

# Attempt to aggregate to 4-hour interval, with the effects of DST
pedestrian %>%
  group_by_key() %>%
  index_by(Date_Time4 = ~ lubridate::floor_date(., "4 hour")) %>%
  summarise(Total_Count = sum(Count))
#> # A tsibble: 16,512 x 3 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor         Date_Time4          Total_Count
#>    <chr>          <dttm>                    <int>
#>  1 Birrarung Marr 2015-01-01 00:00:00        3287
#>  2 Birrarung Marr 2015-01-01 04:00:00         316
#>  3 Birrarung Marr 2015-01-01 08:00:00         995
#>  4 Birrarung Marr 2015-01-01 12:00:00        2117
#>  5 Birrarung Marr 2015-01-01 16:00:00        1829
#>  6 Birrarung Marr 2015-01-01 20:00:00         406
#>  7 Birrarung Marr 2015-01-02 00:00:00          44
#>  8 Birrarung Marr 2015-01-02 04:00:00         555
#>  9 Birrarung Marr 2015-01-02 08:00:00        1193
#> 10 Birrarung Marr 2015-01-02 12:00:00         822
#> # ℹ 16,502 more rows

library(lubridate, warn.conflicts = FALSE)
# Annual trips by Region and State
tourism %>%
  index_by(Year = ~ year(.)) %>%
  group_by(Region, State) %>%
  summarise(Total = sum(Trips))
#> # A tsibble: 1,520 x 4 [1Y]
#> # Key:       Region, State [76]
#> # Groups:    Region [76]
#>    Region   State            Year Total
#>    <chr>    <chr>           <dbl> <dbl>
#>  1 Adelaide South Australia  1998 2226.
#>  2 Adelaide South Australia  1999 2218.
#>  3 Adelaide South Australia  2000 2418.
#>  4 Adelaide South Australia  2001 2264.
#>  5 Adelaide South Australia  2002 2275.
#>  6 Adelaide South Australia  2003 2203.
#>  7 Adelaide South Australia  2004 2437.
#>  8 Adelaide South Australia  2005 2034.
#>  9 Adelaide South Australia  2006 2225.
#> 10 Adelaide South Australia  2007 2317.
#> # ℹ 1,510 more rows

# Rounding to financial year, using a custom function
financial_year <- function(date) {
  year <- year(date)
  ifelse(quarter(date) <= 2, year, year + 1)
}
tourism %>%
  index_by(Year = ~ financial_year(.)) %>%
  summarise(Total = sum(Trips))
#> Error in mutate(ungrp, `:=`(!!idx2, f(!!sym(idx)))): ℹ In argument: `Year = f(Quarter)`.
#> Caused by error in `financial_year()`:
#> ! could not find function "financial_year"
```
