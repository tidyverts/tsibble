# or other tidyselect-compatible functions, `key_vars()` acts as a selection helper that automatically selects all key columns from the tsibble.

or other tidyselect-compatible functions, `key_vars()` acts as a
selection helper that automatically selects all key columns from the
tsibble.

## Usage

``` r
key(x)

key_vars(x)
```

## Arguments

- x:

  A tsibble.

## Examples

``` r
key(pedestrian)
#> [[1]]
#> Sensor
#> 
key_vars(pedestrian)
#> [1] "Sensor"

key(tourism)
#> [[1]]
#> Region
#> 
#> [[2]]
#> State
#> 
#> [[3]]
#> Purpose
#> 
key_vars(tourism)
#> [1] "Region"  "State"   "Purpose"

# Use as a tidyselect helper
library(dplyr)
tourism %>% select(index_var(), key_vars())
#> # A tsibble: 24,320 x 4 [1Q]
#> # Key:       Region, State, Purpose [304]
#>    Quarter Region   State           Purpose 
#>      <qtr> <chr>    <chr>           <chr>   
#>  1 1998 Q1 Adelaide South Australia Business
#>  2 1998 Q2 Adelaide South Australia Business
#>  3 1998 Q3 Adelaide South Australia Business
#>  4 1998 Q4 Adelaide South Australia Business
#>  5 1999 Q1 Adelaide South Australia Business
#>  6 1999 Q2 Adelaide South Australia Business
#>  7 1999 Q3 Adelaide South Australia Business
#>  8 1999 Q4 Adelaide South Australia Business
#>  9 2000 Q1 Adelaide South Australia Business
#> 10 2000 Q2 Adelaide South Australia Business
#> # ℹ 24,310 more rows

# Combine with other tidyselect functions
tourism %>% relocate(key_vars(), .after = last_col())
#> # A tsibble: 24,320 x 5 [1Q]
#> # Key:       Region, State, Purpose [304]
#>    Quarter Trips Region   State           Purpose 
#>      <qtr> <dbl> <chr>    <chr>           <chr>   
#>  1 1998 Q1  135. Adelaide South Australia Business
#>  2 1998 Q2  110. Adelaide South Australia Business
#>  3 1998 Q3  166. Adelaide South Australia Business
#>  4 1998 Q4  127. Adelaide South Australia Business
#>  5 1999 Q1  137. Adelaide South Australia Business
#>  6 1999 Q2  200. Adelaide South Australia Business
#>  7 1999 Q3  169. Adelaide South Australia Business
#>  8 1999 Q4  134. Adelaide South Australia Business
#>  9 2000 Q1  154. Adelaide South Australia Business
#> 10 2000 Q2  169. Adelaide South Australia Business
#> # ℹ 24,310 more rows
```
