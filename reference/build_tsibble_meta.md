# Low-level & high-performance constructor for a tsibble object

**\[experimental\]**

`build_tsibble_meta()` does much less checks than
[`build_tsibble()`](https://tsibble.tidyverts.org/reference/build_tsibble.md)
for high performance.

## Usage

``` r
build_tsibble_meta(
  x,
  key_data = NULL,
  index,
  index2,
  ordered = NULL,
  interval = NULL
)
```

## Arguments

- x:

  A `data.frame`, `tbl_df`, `tbl_ts`, or other tabular objects.

- key_data:

  A data frame containing key variables and `.rows`. When a data frame
  is supplied, the argument `key` will be ignored.

- index, index2:

  Quoted variable name.

- ordered:

  `TRUE` suggests to skip the ordering as `x` in the correct order.
  `FALSE` checks the ordering and may give a warning.

- interval:

  `TRUE` automatically calculates the interval, and `FALSE` for
  irregular interval. Use the specified interval via
  [`new_interval()`](https://tsibble.tidyverts.org/reference/new-interval.md)
  as is.
