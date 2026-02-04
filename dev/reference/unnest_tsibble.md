# Unnest a data frame consisting of tsibbles to a tsibble

**\[deprecated\]**

## Usage

``` r
unnest_tsibble(data, cols, key = NULL, validate = TRUE)
```

## Arguments

- data:

  A data frame contains homogenous tsibbles in the list-columns.

- cols:

  Names of columns to unnest.

- key:

  Variable(s) that uniquely determine time indices. `NULL` for empty
  key, and [`c()`](https://rdrr.io/r/base/c.html) for multiple
  variables. It works with tidy selector (e.g.
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html)).

- validate:

  `TRUE` suggests to verify that each key or each combination of key
  variables leads to unique time indices (i.e. a valid tsibble). If you
  are sure that it's a valid input, specify `FALSE` to skip the checks.
