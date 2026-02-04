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

## Examples

``` r
index(pedestrian)
#> Date_Time
index_var(pedestrian)
#> [1] "Date_Time"
```
