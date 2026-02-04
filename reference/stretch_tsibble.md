# Perform stretching windows on a tsibble by row

**\[questioning\]**

## Usage

``` r
stretch_tsibble(.x, .step = 1, .init = 1, .id = ".id")
```

## Arguments

- .x:

  A tsibble.

- .step:

  A positive integer for incremental step.

- .init:

  A positive integer for an initial window size.

- .id:

  A character naming the new column `.id` containing the partition.

## Rolling tsibble

[`slide_tsibble()`](https://tsibble.tidyverts.org/reference/slide_tsibble.md),
[`tile_tsibble()`](https://tsibble.tidyverts.org/reference/tile_tsibble.md),
and `stretch_tsibble()` provide fast and shorthand for rolling over a
tsibble by observations. That said, if the supplied tsibble has time
gaps, these rolling helpers will ignore those gaps and proceed.

They are useful for preparing the tsibble for time series cross
validation. They all return a tsibble including a new column `.id` as
part of the key. The output dimension will increase considerably with
[`slide_tsibble()`](https://tsibble.tidyverts.org/reference/slide_tsibble.md)
and `stretch_tsibble()`, which is likely to run out of memory when the
data is large.

## See also

Other rolling tsibble:
[`slide_tsibble()`](https://tsibble.tidyverts.org/reference/slide_tsibble.md),
[`tile_tsibble()`](https://tsibble.tidyverts.org/reference/tile_tsibble.md)

## Examples

``` r
harvest <- tsibble(
  year = rep(2010:2012, 2),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
harvest %>%
  stretch_tsibble()
#> # A tsibble: 12 x 4 [1Y]
#> # Key:       .id, fruit [6]
#>     year fruit   kilo   .id
#>    <int> <chr>  <int> <int>
#>  1  2010 cherry     4     1
#>  2  2010 kiwi      10     1
#>  3  2010 cherry     4     2
#>  4  2011 cherry     1     2
#>  5  2010 kiwi      10     2
#>  6  2011 kiwi       3     2
#>  7  2010 cherry     4     3
#>  8  2011 cherry     1     3
#>  9  2012 cherry     6     3
#> 10  2010 kiwi      10     3
#> 11  2011 kiwi       3     3
#> 12  2012 kiwi       8     3
```
