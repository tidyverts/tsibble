# Return measured variables

Return measured variables

Return measured variables

## Usage

``` r
measures(x)

measures(x)

measured_vars(x)
```

## Arguments

- x:

  A `tbl_ts`.

## Details

`measures()` returns a list of symbols; `measured_vars()` gives a
character vector.

When used inside tidyverse functions like
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
or other tidyselect-compatible functions, `measured_vars()` acts as a
selection helper that automatically selects all measured variables
(non-key, non-index columns) from the tsibble.

`measures()` returns a list of symbols; `measured_vars()` gives a
character vector.

When used inside tidyverse functions like
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
or other tidyselect-compatible functions, `measured_vars()` acts as a
selection helper that automatically selects all measured variables
(non-key, non-index columns) from the tsibble.

## Examples

``` r
measures(pedestrian)
#> [[1]]
#> Date
#> 
#> [[2]]
#> Time
#> 
#> [[3]]
#> Count
#> 
measures(tourism)
#> [[1]]
#> Trips
#> 

measured_vars(pedestrian)
#> [1] "Date"  "Time"  "Count"
measured_vars(tourism)
#> [1] "Trips"

# Use as a tidyselect helper to select only measured variables
library(dplyr)
tourism %>% select(measured_vars())
#> # A tsibble: 24,320 x 5 [1Q]
#> # Key:       Region, State, Purpose [304]
#>    Trips Quarter Region   State           Purpose 
#>    <dbl>   <qtr> <chr>    <chr>           <chr>   
#>  1  135. 1998 Q1 Adelaide South Australia Business
#>  2  110. 1998 Q2 Adelaide South Australia Business
#>  3  166. 1998 Q3 Adelaide South Australia Business
#>  4  127. 1998 Q4 Adelaide South Australia Business
#>  5  137. 1999 Q1 Adelaide South Australia Business
#>  6  200. 1999 Q2 Adelaide South Australia Business
#>  7  169. 1999 Q3 Adelaide South Australia Business
#>  8  134. 1999 Q4 Adelaide South Australia Business
#>  9  154. 2000 Q1 Adelaide South Australia Business
#> 10  169. 2000 Q2 Adelaide South Australia Business
#> # ℹ 24,310 more rows

# Combine with key and index selectors
tourism %>% select(measured_vars(), key_vars(), index_var())
#> # A tsibble: 24,320 x 5 [1Q]
#> # Key:       Region, State, Purpose [304]
#>    Trips Region   State           Purpose  Quarter
#>    <dbl> <chr>    <chr>           <chr>      <qtr>
#>  1  135. Adelaide South Australia Business 1998 Q1
#>  2  110. Adelaide South Australia Business 1998 Q2
#>  3  166. Adelaide South Australia Business 1998 Q3
#>  4  127. Adelaide South Australia Business 1998 Q4
#>  5  137. Adelaide South Australia Business 1999 Q1
#>  6  200. Adelaide South Australia Business 1999 Q2
#>  7  169. Adelaide South Australia Business 1999 Q3
#>  8  134. Adelaide South Australia Business 1999 Q4
#>  9  154. Adelaide South Australia Business 2000 Q1
#> 10  169. Adelaide South Australia Business 2000 Q2
#> # ℹ 24,310 more rows

# Use with other tidyselect functions
pedestrian %>% select(measured_vars(), where(is.numeric))
#> # A tsibble: 66,037 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Date        Time Count Date_Time           Sensor        
#>    <date>     <int> <int> <dttm>              <chr>         
#>  1 2015-01-01     0  1630 2015-01-01 00:00:00 Birrarung Marr
#>  2 2015-01-01     1   826 2015-01-01 01:00:00 Birrarung Marr
#>  3 2015-01-01     2   567 2015-01-01 02:00:00 Birrarung Marr
#>  4 2015-01-01     3   264 2015-01-01 03:00:00 Birrarung Marr
#>  5 2015-01-01     4   139 2015-01-01 04:00:00 Birrarung Marr
#>  6 2015-01-01     5    77 2015-01-01 05:00:00 Birrarung Marr
#>  7 2015-01-01     6    44 2015-01-01 06:00:00 Birrarung Marr
#>  8 2015-01-01     7    56 2015-01-01 07:00:00 Birrarung Marr
#>  9 2015-01-01     8   113 2015-01-01 08:00:00 Birrarung Marr
#> 10 2015-01-01     9   166 2015-01-01 09:00:00 Birrarung Marr
#> # ℹ 66,027 more rows
measures(pedestrian)
#> [[1]]
#> Date
#> 
#> [[2]]
#> Time
#> 
#> [[3]]
#> Count
#> 
measures(tourism)
#> [[1]]
#> Trips
#> 

measured_vars(pedestrian)
#> [1] "Date"  "Time"  "Count"
measured_vars(tourism)
#> [1] "Trips"

# Use as a tidyselect helper to select measured variables
library(dplyr)
tourism %>% select(measured_vars(), key_vars(), index_var())
#> # A tsibble: 24,320 x 5 [1Q]
#> # Key:       Region, State, Purpose [304]
#>    Trips Region   State           Purpose  Quarter
#>    <dbl> <chr>    <chr>           <chr>      <qtr>
#>  1  135. Adelaide South Australia Business 1998 Q1
#>  2  110. Adelaide South Australia Business 1998 Q2
#>  3  166. Adelaide South Australia Business 1998 Q3
#>  4  127. Adelaide South Australia Business 1998 Q4
#>  5  137. Adelaide South Australia Business 1999 Q1
#>  6  200. Adelaide South Australia Business 1999 Q2
#>  7  169. Adelaide South Australia Business 1999 Q3
#>  8  134. Adelaide South Australia Business 1999 Q4
#>  9  154. Adelaide South Australia Business 2000 Q1
#> 10  169. Adelaide South Australia Business 2000 Q2
#> # ℹ 24,310 more rows

# Use with other tidyselect functions
pedestrian %>% select(measured_vars(), where(is.numeric))
#> # A tsibble: 66,037 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Date        Time Count Date_Time           Sensor        
#>    <date>     <int> <int> <dttm>              <chr>         
#>  1 2015-01-01     0  1630 2015-01-01 00:00:00 Birrarung Marr
#>  2 2015-01-01     1   826 2015-01-01 01:00:00 Birrarung Marr
#>  3 2015-01-01     2   567 2015-01-01 02:00:00 Birrarung Marr
#>  4 2015-01-01     3   264 2015-01-01 03:00:00 Birrarung Marr
#>  5 2015-01-01     4   139 2015-01-01 04:00:00 Birrarung Marr
#>  6 2015-01-01     5    77 2015-01-01 05:00:00 Birrarung Marr
#>  7 2015-01-01     6    44 2015-01-01 06:00:00 Birrarung Marr
#>  8 2015-01-01     7    56 2015-01-01 07:00:00 Birrarung Marr
#>  9 2015-01-01     8   113 2015-01-01 08:00:00 Birrarung Marr
#> 10 2015-01-01     9   166 2015-01-01 09:00:00 Birrarung Marr
#> # ℹ 66,027 more rows
```
