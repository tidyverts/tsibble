# Perform sliding windows on a tsibble by row

**\[questioning\]**

## Usage

``` r
slide_tsibble(.x, .size = 1, .step = 1, .id = ".id")
```

## Arguments

- .x:

  A tsibble.

- .size:

  A positive integer for window size.

- .step:

  A positive integer for calculating at every specified step instead of
  every single step.

- .id:

  A character naming the new column `.id` containing the partition.

## Rolling tsibble

`slide_tsibble()`,
[`tile_tsibble()`](https://tsibble.tidyverts.org/dev/reference/tile_tsibble.md),
and
[`stretch_tsibble()`](https://tsibble.tidyverts.org/dev/reference/stretch_tsibble.md)
provide fast and shorthand for rolling over a tsibble by observations.
That said, if the supplied tsibble has time gaps, these rolling helpers
will ignore those gaps and proceed.

They are useful for preparing the tsibble for time series cross
validation. They all return a tsibble including a new column `.id` as
part of the key. The output dimension will increase considerably with
`slide_tsibble()` and
[`stretch_tsibble()`](https://tsibble.tidyverts.org/dev/reference/stretch_tsibble.md),
which is likely to run out of memory when the data is large.

## See also

Other rolling tsibble:
[`stretch_tsibble()`](https://tsibble.tidyverts.org/dev/reference/stretch_tsibble.md),
[`tile_tsibble()`](https://tsibble.tidyverts.org/dev/reference/tile_tsibble.md)

## Examples

``` r
harvest <- tsibble(
  year = rep(2010:2012, 2),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest %>%
  slide_tsibble(.size = 2)
#> # A tsibble: 8 x 4 [1Y]
#> # Key:       .id, fruit [4]
#>    year fruit   kilo   .id
#>   <int> <chr>  <int> <int>
#> 1  2010 cherry     8     1
#> 2  2011 cherry     4     1
#> 3  2010 kiwi       6     1
#> 4  2011 kiwi       1     1
#> 5  2011 cherry     4     2
#> 6  2012 cherry     3     2
#> 7  2011 kiwi       1     2
#> 8  2012 kiwi       7     2
```
