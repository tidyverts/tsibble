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

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Variable(s) that uniquely determine time indices. `NULL` for an empty
  key, unquoted column names (e.g. `x`) for a single variable, and
  [`c()`](https://rdrr.io/r/base/c.html) for multiple variables (e.g.
  `c(x, y)`). This argument also supports [tidy-select
  expressions](https://tidyselect.r-lib.org/reference/language.html),
  e.g.
  [`dplyr::starts_with()`](https://dplyr.tidyverse.org/reference/reexports.html),
  [`dplyr::all_of()`](https://dplyr.tidyverse.org/reference/reexports.html).

- validate:

  `TRUE` suggests to verify that each key or each combination of key
  variables leads to unique time indices (i.e. a valid tsibble). If you
  are sure that it's a valid input, specify `FALSE` to skip the checks.
