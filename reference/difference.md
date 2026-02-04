# Lagged differences

**\[stable\]**

## Usage

``` r
difference(x, lag = 1, differences = 1, default = NA, order_by = NULL)
```

## Arguments

- x:

  A vector

- lag:

  A positive integer indicating which lag to use.

- differences:

  A positive integer indicating the order of the difference.

- default:

  The value used to pad `x` back to its original size after the lag or
  lead has been applied. The default, `NULL`, pads with a missing value.
  If supplied, this must be a vector with size 1, which will be cast to
  the type of `x`.

- order_by:

  An optional secondary vector that defines the ordering to use when
  applying the lag or lead to `x`. If supplied, this must be the same
  size as `x`.

## Value

A numeric vector of the same length as `x`.

## See also

[dplyr::lead](https://dplyr.tidyverse.org/reference/lead-lag.html) and
[dplyr::lag](https://dplyr.tidyverse.org/reference/lead-lag.html)

## Examples

``` r
# examples from base
difference(1:10, 2)
#>  [1] NA NA  2  2  2  2  2  2  2  2
difference(1:10, 2, 2)
#>  [1] NA NA NA NA  0  0  0  0  0  0
x <- cumsum(cumsum(1:10))
difference(x, lag = 2)
#>  [1]  NA  NA   9  16  25  36  49  64  81 100
difference(x, differences = 2)
#>  [1] NA NA  3  4  5  6  7  8  9 10
# Use order_by if data not already ordered (example from dplyr)
library(dplyr, warn.conflicts = FALSE)
tsbl <- tsibble(year = 2000:2005, value = (0:5)^2, index = year)
scrambled <- tsbl %>% slice(sample(nrow(tsbl)))
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.

wrong <- mutate(scrambled, diff = difference(value))
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.
arrange(wrong, year)
#> # A tsibble: 6 x 3 [1Y]
#>    year value  diff
#>   <int> <dbl> <dbl>
#> 1  2000     0    NA
#> 2  2001     1    -3
#> 3  2002     4     4
#> 4  2003     9    -7
#> 5  2004    16    15
#> 6  2005    25    16

right <- mutate(scrambled, diff = difference(value, order_by = year))
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.
#> Warning: Current temporal ordering may yield unexpected results.
#> ℹ Suggest to sort by ``, `year` first.
arrange(right, year)
#> # A tsibble: 6 x 3 [1Y]
#>    year value  diff
#>   <int> <dbl> <dbl>
#> 1  2000     0    NA
#> 2  2001     1     1
#> 3  2002     4     3
#> 4  2003     9     5
#> 5  2004    16     7
#> 6  2005    25     9
```
