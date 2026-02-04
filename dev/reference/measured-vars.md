# Return measured variables

Return measured variables

## Usage

``` r
measures(x)

measured_vars(x)
```

## Arguments

- x:

  A `tbl_ts`.

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
```
