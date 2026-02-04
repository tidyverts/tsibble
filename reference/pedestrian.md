# Pedestrian counts in the city of Melbourne

A dataset containing the hourly pedestrian counts from 2015-01-01 to
2016-12-31 at 4 sensors in the city of Melbourne.

## Usage

``` r
pedestrian
```

## Format

A tsibble with 66,071 rows and 5 variables:

- **Sensor**: Sensor names (key)

- **Date_Time**: Date time when the pedestrian counts are recorded
  (index)

- **Date**: Date when the pedestrian counts are recorded

- **Time**: Hour associated with Date_Time

- **Counts**: Hourly pedestrian counts

## Examples

``` r
library(dplyr)
data(pedestrian)
# make implicit missingness to be explicit ----
pedestrian %>% fill_gaps()
#> # A tsibble: 69,048 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
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
#> # ℹ 69,038 more rows
# compute daily maximum counts across sensors ----
pedestrian %>%
  group_by_key() %>%
  index_by(Date) %>% # group by Date and use it as new index
  summarise(MaxC = max(Count))
#> # A tsibble: 2,752 x 3 [1D]
#> # Key:       Sensor [4]
#>    Sensor         Date        MaxC
#>    <chr>          <date>     <int>
#>  1 Birrarung Marr 2015-01-01  1630
#>  2 Birrarung Marr 2015-01-02   352
#>  3 Birrarung Marr 2015-01-03   226
#>  4 Birrarung Marr 2015-01-04   852
#>  5 Birrarung Marr 2015-01-05  1427
#>  6 Birrarung Marr 2015-01-06   937
#>  7 Birrarung Marr 2015-01-07   708
#>  8 Birrarung Marr 2015-01-08   568
#>  9 Birrarung Marr 2015-01-09  1629
#> 10 Birrarung Marr 2015-01-10  2439
#> # ℹ 2,742 more rows
```
