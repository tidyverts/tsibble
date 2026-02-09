# Return index variable from a tsibble

Return index variable from a tsibble

## Usage

``` r
index(x)

index_var(x)

index2(x)

index2_var(x)
```

## Arguments

- x:

  A tsibble object.

## Details

`index()` returns a symbol; `index_var()` gives a character string.

When used inside tidyverse functions like
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
or other tidyselect-compatible functions, `index_var()` acts as a
selection helper that automatically selects the index column from the
tsibble.

## Examples

``` r
index(pedestrian)
#> Date_Time
index_var(pedestrian)
#> [1] "Date_Time"

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

# Use with relocate
tourism %>% relocate(index_var(), .after = key_vars())
#> # A tsibble: 24,320 x 5 [1Q]
#> # Key:       Region, State, Purpose [304]
#>    Region   State           Purpose  Quarter Trips
#>    <chr>    <chr>           <chr>      <qtr> <dbl>
#>  1 Adelaide South Australia Business 1998 Q1  135.
#>  2 Adelaide South Australia Business 1998 Q2  110.
#>  3 Adelaide South Australia Business 1998 Q3  166.
#>  4 Adelaide South Australia Business 1998 Q4  127.
#>  5 Adelaide South Australia Business 1999 Q1  137.
#>  6 Adelaide South Australia Business 1999 Q2  200.
#>  7 Adelaide South Australia Business 1999 Q3  169.
#>  8 Adelaide South Australia Business 1999 Q4  134.
#>  9 Adelaide South Australia Business 2000 Q1  154.
#> 10 Adelaide South Australia Business 2000 Q2  169.
#> # ℹ 24,310 more rows
```
