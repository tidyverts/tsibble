# Does a tsibble have implicit gaps in time?

Does a tsibble have implicit gaps in time?

## Usage

``` r
has_gaps(.data, .full = FALSE, .name = ".gaps", .start = NULL, .end = NULL)
```

## Arguments

- .data:

  A tsibble.

- .full:

  - `FALSE` inserts `NA` for each keyed unit within its own period.

  - `TRUE` fills `NA` over the entire time span of the data (a.k.a.
    fully balanced panel).

  - [`start()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    starting point (i.e. `min(<index>)`) across units.

  - [`end()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    ending point (i.e. `max(<index>)`) across units.

- .name:

  Strings to name new columns.

- .start, .end:

  Set custom starting/ending time that allows to expand the existing
  time spans.

## Value

A tibble contains "key" variables and new column `.gaps` of
`TRUE`/`FALSE`.

## See also

Other implicit gaps handling:
[`count_gaps()`](https://tsibble.tidyverts.org/reference/count_gaps.md),
[`fill_gaps()`](https://tsibble.tidyverts.org/reference/fill_gaps.md),
[`scan_gaps()`](https://tsibble.tidyverts.org/reference/scan_gaps.md)

## Examples

``` r
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 2012, 2013),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)
has_gaps(harvest)
#> # A tibble: 2 × 2
#>   fruit  .gaps
#>   <chr>  <lgl>
#> 1 cherry FALSE
#> 2 kiwi   TRUE 
has_gaps(harvest, .full = TRUE)
#> # A tibble: 2 × 2
#>   fruit  .gaps
#>   <chr>  <lgl>
#> 1 cherry TRUE 
#> 2 kiwi   TRUE 
has_gaps(harvest, .full = start())
#> # A tibble: 2 × 2
#>   fruit  .gaps
#>   <chr>  <lgl>
#> 1 cherry TRUE 
#> 2 kiwi   TRUE 
has_gaps(harvest, .full = end())
#> # A tibble: 2 × 2
#>   fruit  .gaps
#>   <chr>  <lgl>
#> 1 cherry FALSE
#> 2 kiwi   TRUE 
```
