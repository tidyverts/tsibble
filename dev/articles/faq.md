# Frequently Asked Questions and Answers

``` r
library(dplyr)
library(tsibble)
library(lubridate)
```

> *Error: “Can’t obtain the interval due to the mismatched index
> class.”*

> *I have monthly data and coerce it to a tsibble. Why does tsibble give
> one-day interval `[1D]` instead of one-month `[1M]`?*

``` r
mth <- make_date("2018") + months(0:3)
tsibble(mth = mth, index = mth)
#> # A tsibble: 4 x 1 [1D]
#>   mth       
#>   <date>    
#> 1 2018-01-01
#> 2 2018-02-01
#> 3 2018-03-01
#> 4 2018-04-01
```

The interval depends on the index class. It is unclear in this situation
to tell if it’s daily data with implicit missingness or it’s monthly
data. If using `Date` underlying monthly data, each month could range
from 28 to 31 days, which isn’t regularly spaced. But class `yearmonth`
puts emphasis on 12 months per year, which is clearly regularly spaced
and the accurate representation for aggregations over months. This
applies to `POSIXct` for sub-daily data, `Date` for daily, `yearquarter`
for quarterly, and etc. If you encounter this error “Can’t obtain the
interval due to mismatched index class.”, it’s the same underlying
issue.

``` r
tsibble(mth = yearmonth(mth), index = mth)
#> # A tsibble: 4 x 1 [1M]
#>        mth
#>      <mth>
#> 1 2018 Jan
#> 2 2018 Feb
#> 3 2018 Mar
#> 4 2018 Apr
```

------------------------------------------------------------------------

> *Does tsibble respect time zones?*

Yes, tsibble respects time zones throughout the package. All index
functions including
[`yearweek()`](https://tsibble.tidyverts.org/dev/reference/year-week.md),
[`yearmonth()`](https://tsibble.tidyverts.org/dev/reference/year-month.md),
[`yearquarter()`](https://tsibble.tidyverts.org/dev/reference/year-quarter.md),
and
[`time_in()`](https://tsibble.tidyverts.org/dev/reference/time_in.md)
take care of time zones, and will NOT convert to “UTC”. The interval
obtained from the data also respects the time zone, by converting to
seconds. The following example demonstrates how tsibble handles daylight
savings.

``` r
x <- ymd_h("2015-04-05 01", tz = "Australia/Melbourne")
# base arithmetic respect tz
tsibble(time = x + (c(0, 3, 6, 9)) * 60 * 60, index = time)
#> # A tsibble: 4 x 1 [3h] <Australia/Melbourne>
#>   time               
#>   <dttm>             
#> 1 2015-04-05 01:00:00
#> 2 2015-04-05 03:00:00
#> 3 2015-04-05 06:00:00
#> 4 2015-04-05 09:00:00
# lubridate arithmetic doesn't respect tz
tsibble(time = x + hours(c(0, 3, 6, 9)), index = time)
#> # A tsibble: 4 x 1 [1h] <Australia/Melbourne>
#>   time               
#>   <dttm>             
#> 1 2015-04-05 01:00:00
#> 2 2015-04-05 04:00:00
#> 3 2015-04-05 07:00:00
#> 4 2015-04-05 10:00:00
```

I would say both are correct. The displayed interval may suggest the
actual time is different from what you think it is.

------------------------------------------------------------------------

> *I have multiple units measured at different time intervals. Can I put
> them into one tsibble?*

``` r
tsbl1 <- tsibble(
  time = make_datetime(2018) + hours(0:3),
  station = "A",
  index = time, key = station
) %>% print()
#> # A tsibble: 4 x 2 [1h] <UTC>
#> # Key:       station [1]
#>   time                station
#>   <dttm>              <chr>  
#> 1 2018-01-01 00:00:00 A      
#> 2 2018-01-01 01:00:00 A      
#> 3 2018-01-01 02:00:00 A      
#> 4 2018-01-01 03:00:00 A
tsbl2 <- tsibble(
  time = make_datetime(2018) + minutes(seq(0, 90, by = 30)),
  station = "B",
  index = time, key = station
) %>% print()
#> # A tsibble: 4 x 2 [30m] <UTC>
#> # Key:       station [1]
#>   time                station
#>   <dttm>              <chr>  
#> 1 2018-01-01 00:00:00 B      
#> 2 2018-01-01 00:30:00 B      
#> 3 2018-01-01 01:00:00 B      
#> 4 2018-01-01 01:30:00 B
bind_rows(tsbl1, tsbl2)
#> # A tsibble: 8 x 2 [30m] <UTC>
#> # Key:       station [2]
#>   time                station
#>   <dttm>              <chr>  
#> 1 2018-01-01 00:00:00 A      
#> 2 2018-01-01 01:00:00 A      
#> 3 2018-01-01 02:00:00 A      
#> 4 2018-01-01 03:00:00 A      
#> 5 2018-01-01 00:00:00 B      
#> 6 2018-01-01 00:30:00 B      
#> 7 2018-01-01 01:00:00 B      
#> 8 2018-01-01 01:30:00 B
```

Certainly you can. But tsibble only allows for one interval, because
station `A` is thought of as time gaps involved. If you want to analyse
them differently, it is recommended to have separate tsibbles instead.

------------------------------------------------------------------------

> *I have multiple units measured at the same time interval. But the
> tsibble interval doesn’t look correct.*

``` r
x <- make_datetime(2018) + minutes(0:1)
tbl <- tibble(  
  time = c(x, x + minutes(15)),
  station = rep(c("A", "B"), 2)
)
as_tsibble(tbl, index = time, key = station)
#> # A tsibble: 4 x 2 [1m] <UTC>
#> # Key:       station [2]
#>   time                station
#>   <dttm>              <chr>  
#> 1 2018-01-01 00:00:00 A      
#> 2 2018-01-01 00:15:00 A      
#> 3 2018-01-01 00:01:00 B      
#> 4 2018-01-01 00:16:00 B
```

Each station shares the common 15-minute interval, but the date-times
don’t align. Rounding them is a quick way to fix it, if binning time
doesn’t matter to the analysis. If it does, please organise them in
different tables.

``` r
tbl %>% 
  mutate(time = floor_date(time, unit = "15 mins")) %>% 
  as_tsibble(index = time, key = station)
#> # A tsibble: 4 x 2 [15m] <UTC>
#> # Key:       station [2]
#>   time                station
#>   <dttm>              <chr>  
#> 1 2018-01-01 00:00:00 A      
#> 2 2018-01-01 00:15:00 A      
#> 3 2018-01-01 00:00:00 B      
#> 4 2018-01-01 00:15:00 B
```

If it’s event data, each event couples with a precise time stamp, and
most likely you need `regular = FALSE` for an irregularly-spaced
tsibble.
