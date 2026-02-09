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

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Variable(s) that uniquely determine time indices. `NULL` for an empty
  key, unquoted column names (e.g. `x`) for a single variable, and
  [`c()`](https://rdrr.io/r/base/c.html) for multiple variables (e.g.
  `c(x, y)`). This argument also supports [tidy-select
  expressions](https://tidyselect.r-lib.org/reference/language.html),
  e.g.
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html),
  [`dplyr::all_of()`](https://dplyr.tidyverse.org/reference/reexports.html).

- index:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  A variable that contains time indices. This is commonly an unquoted
  column name (e.g. `t`), but it can also be a [tidy-select
  expression](https://tidyselect.r-lib.org/reference/language.html).

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
