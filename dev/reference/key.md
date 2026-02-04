# Return key variables

`key()` returns a list of symbols; `key_vars()` gives a character
vector.

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
```
