# Test duplicated observations determined by key and index variables

**\[stable\]**

- `is_duplicated()`: a logical scalar if the data exist duplicated
  observations.

- `are_duplicated()`: a logical vector, the same length as the row
  number of `data`.

- `duplicates()`: identical key-index data entries.

## Usage

``` r
is_duplicated(data, key = NULL, index)

are_duplicated(data, key = NULL, index, from_last = FALSE)

duplicates(data, key = NULL, index)
```

## Arguments

- data:

  A data frame for creating a tsibble.

- key:

  Variable(s) that uniquely determine time indices. `NULL` for empty
  key, and [`c()`](https://rdrr.io/r/base/c.html) for multiple
  variables. It works with tidy selector (e.g.
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html)).

- index:

  A variable to specify the time index variable.

- from_last:

  `TRUE` does the duplication check from the last of identical elements.

## Examples

``` r
harvest <- tibble(
  year = c(2010, 2011, 2013, 2011, 2012, 2014, 2014),
  fruit = c(rep(c("kiwi", "cherry"), each = 3), "cherry"),
  kilo = sample(1:10, size = 7)
)
is_duplicated(harvest, key = fruit, index = year)
#> [1] TRUE
are_duplicated(harvest, key = fruit, index = year)
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
are_duplicated(harvest, key = fruit, index = year, from_last = TRUE)
#> [1] FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
duplicates(harvest, key = fruit, index = year)
#> # A tibble: 2 × 3
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2014 cherry     8
#> 2  2014 cherry     5
```
