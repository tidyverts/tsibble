# Add custom index support for a tsibble

**\[stable\]**

S3 method to add an index type support for a tsibble.

## Usage

``` r
index_valid(x)
```

## Arguments

- x:

  An object of index type supported by tsibble.

## Value

`TRUE`/`FALSE` or `NA` (unsure)

## Details

This method is primarily used for adding an index type support in
[as_tsibble](https://tsibble.tidyverts.org/reference/as-tsibble.md).

## See also

[interval_pull](https://tsibble.tidyverts.org/reference/interval-pull.md)
for obtaining interval for regularly spaced time.

## Examples

``` r
index_valid(seq(as.Date("2017-01-01"), as.Date("2017-01-10"), by = 1))
#> [1] TRUE
```
