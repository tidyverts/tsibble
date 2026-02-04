# Key metadata

Key metadata

## Usage

``` r
key_data(.data)

key_rows(.data)

key_size(x)

n_keys(x)
```

## Arguments

- .data, x:

  A tsibble

## See also

[dplyr::group_data](https://dplyr.tidyverse.org/reference/group_data.html)

## Examples

``` r
key_data(pedestrian)
#> # A tibble: 4 × 2
#>   Sensor                              .rows
#> * <chr>                         <list<int>>
#> 1 Birrarung Marr                   [14,566]
#> 2 Bourke Street Mall (North)       [16,414]
#> 3 QV Market-Elizabeth St (West)    [17,518]
#> 4 Southern Cross Station           [17,539]
```
