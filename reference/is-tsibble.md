# If the object is a tsibble

**\[stable\]**

## Usage

``` r
is_tsibble(x)

is_grouped_ts(x)
```

## Arguments

- x:

  An object.

## Value

TRUE if the object inherits from the tbl_ts class.

## Examples

``` r
# A tibble is not a tsibble ----
tbl <- tibble(
  date = seq(as.Date("2017-10-01"), as.Date("2017-10-31"), by = 1),
  value = rnorm(31)
)
is_tsibble(tbl)
#> [1] FALSE

# A tsibble ----
tsbl <- as_tsibble(tbl, index = date)
is_tsibble(tsbl)
#> [1] TRUE
```
