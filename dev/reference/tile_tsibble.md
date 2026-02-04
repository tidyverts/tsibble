# Perform tiling windows on a tsibble by row

**\[questioning\]**

## Usage

``` r
tile_tsibble(.x, .size = 1, .id = ".id")
```

## Arguments

- .x:

  A tsibble.

- .size:

  A positive integer for window size.

- .id:

  A character naming the new column `.id` containing the partition.

## Rolling tsibble

[`slide_tsibble()`](https://tsibble.tidyverts.org/dev/reference/slide_tsibble.md),
`tile_tsibble()`, and
[`stretch_tsibble()`](https://tsibble.tidyverts.org/dev/reference/stretch_tsibble.md)
provide fast and shorthand for rolling over a tsibble by observations.
That said, if the supplied tsibble has time gaps, these rolling helpers
will ignore those gaps and proceed.

They are useful for preparing the tsibble for time series cross
validation. They all return a tsibble including a new column `.id` as
part of the key. The output dimension will increase considerably with
[`slide_tsibble()`](https://tsibble.tidyverts.org/dev/reference/slide_tsibble.md)
and
[`stretch_tsibble()`](https://tsibble.tidyverts.org/dev/reference/stretch_tsibble.md),
which is likely to run out of memory when the data is large.

## See also

Other rolling tsibble:
[`slide_tsibble()`](https://tsibble.tidyverts.org/dev/reference/slide_tsibble.md),
[`stretch_tsibble()`](https://tsibble.tidyverts.org/dev/reference/stretch_tsibble.md)

## Examples

``` r
harvest <- tsibble(
  year = rep(2010:2012, 2),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest %>%
  tile_tsibble(.size = 2)
#> # A tsibble: 6 x 4 [1Y]
#> # Key:       .id, fruit [4]
#>    year fruit   kilo   .id
#>   <int> <chr>  <int> <int>
#> 1  2010 cherry     6     1
#> 2  2011 cherry     5     1
#> 3  2010 kiwi       3     1
#> 4  2011 kiwi       8     1
#> 5  2012 cherry    10     2
#> 6  2012 kiwi       2     2
```
