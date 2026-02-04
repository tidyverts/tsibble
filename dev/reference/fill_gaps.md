# Turn implicit missing values into explicit missing values

**\[stable\]**

## Usage

``` r
fill_gaps(.data, ..., .full = FALSE, .start = NULL, .end = NULL)
```

## Arguments

- .data:

  A tsibble.

- ...:

  A set of name-value pairs. The values provided will only replace
  missing values that were marked as "implicit", and will leave
  previously existing `NA` untouched.

  - empty: filled with default `NA`.

  - filled by values or functions.

- .full:

  - `FALSE` inserts `NA` for each keyed unit within its own period.

  - `TRUE` fills `NA` over the entire time span of the data (a.k.a.
    fully balanced panel).

  - [`start()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    starting point (i.e. `min(<index>)`) across units.

  - [`end()`](https://rdrr.io/r/stats/start.html) pad `NA` to the same
    ending point (i.e. `max(<index>)`) across units.

- .start, .end:

  Set custom starting/ending time that allows to expand the existing
  time spans.

## See also

[tidyr::fill](https://tidyr.tidyverse.org/reference/fill.html),
[tidyr::replace_na](https://tidyr.tidyverse.org/reference/replace_na.html)
for handling missing values `NA`.

Other implicit gaps handling:
[`count_gaps()`](https://tsibble.tidyverts.org/dev/reference/count_gaps.md),
[`has_gaps()`](https://tsibble.tidyverts.org/dev/reference/has_gaps.md),
[`scan_gaps()`](https://tsibble.tidyverts.org/dev/reference/scan_gaps.md)

## Examples

``` r
harvest <- tsibble(
  year = c(2010, 2011, 2013, 2011, 2012, 2014),
  fruit = rep(c("kiwi", "cherry"), each = 3),
  kilo = sample(1:10, size = 6),
  key = fruit, index = year
)

# gaps as default `NA`
fill_gaps(harvest, .full = TRUE)
#> # A tsibble: 10 x 3 [1Y]
#> # Key:       fruit [2]
#>     year fruit   kilo
#>    <dbl> <chr>  <int>
#>  1  2010 cherry    NA
#>  2  2011 cherry     4
#>  3  2012 cherry     7
#>  4  2013 cherry    NA
#>  5  2014 cherry    10
#>  6  2010 kiwi       6
#>  7  2011 kiwi       2
#>  8  2012 kiwi      NA
#>  9  2013 kiwi       1
#> 10  2014 kiwi      NA
fill_gaps(harvest, .full = start())
#> # A tsibble: 9 x 3 [1Y]
#> # Key:       fruit [2]
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2010 cherry    NA
#> 2  2011 cherry     4
#> 3  2012 cherry     7
#> 4  2013 cherry    NA
#> 5  2014 cherry    10
#> 6  2010 kiwi       6
#> 7  2011 kiwi       2
#> 8  2012 kiwi      NA
#> 9  2013 kiwi       1
fill_gaps(harvest, .full = end())
#> # A tsibble: 9 x 3 [1Y]
#> # Key:       fruit [2]
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2011 cherry     4
#> 2  2012 cherry     7
#> 3  2013 cherry    NA
#> 4  2014 cherry    10
#> 5  2010 kiwi       6
#> 6  2011 kiwi       2
#> 7  2012 kiwi      NA
#> 8  2013 kiwi       1
#> 9  2014 kiwi      NA
fill_gaps(harvest, .start = 2009, .end = 2016)
#> # A tsibble: 16 x 3 [1Y]
#> # Key:       fruit [2]
#>     year fruit   kilo
#>    <dbl> <chr>  <int>
#>  1  2009 cherry    NA
#>  2  2010 cherry    NA
#>  3  2011 cherry     4
#>  4  2012 cherry     7
#>  5  2013 cherry    NA
#>  6  2014 cherry    10
#>  7  2015 cherry    NA
#>  8  2016 cherry    NA
#>  9  2009 kiwi      NA
#> 10  2010 kiwi       6
#> 11  2011 kiwi       2
#> 12  2012 kiwi      NA
#> 13  2013 kiwi       1
#> 14  2014 kiwi      NA
#> 15  2015 kiwi      NA
#> 16  2016 kiwi      NA
full_harvest <- fill_gaps(harvest, .full = FALSE)
full_harvest
#> # A tsibble: 8 x 3 [1Y]
#> # Key:       fruit [2]
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2011 cherry     4
#> 2  2012 cherry     7
#> 3  2013 cherry    NA
#> 4  2014 cherry    10
#> 5  2010 kiwi       6
#> 6  2011 kiwi       2
#> 7  2012 kiwi      NA
#> 8  2013 kiwi       1

# replace gaps with a specific value
harvest %>%
  fill_gaps(kilo = 0L)
#> # A tsibble: 8 x 3 [1Y]
#> # Key:       fruit [2]
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2011 cherry     4
#> 2  2012 cherry     7
#> 3  2013 cherry     0
#> 4  2014 cherry    10
#> 5  2010 kiwi       6
#> 6  2011 kiwi       2
#> 7  2012 kiwi       0
#> 8  2013 kiwi       1

# replace gaps using a function by variable
harvest %>%
  fill_gaps(kilo = sum(kilo))
#> # A tsibble: 8 x 3 [1Y]
#> # Key:       fruit [2]
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2011 cherry     4
#> 2  2012 cherry     7
#> 3  2013 cherry    30
#> 4  2014 cherry    10
#> 5  2010 kiwi       6
#> 6  2011 kiwi       2
#> 7  2012 kiwi      30
#> 8  2013 kiwi       1

# replace gaps using a function for each group
harvest %>%
  group_by_key() %>%
  fill_gaps(kilo = sum(kilo))
#> # A tsibble: 8 x 3 [1Y]
#> # Key:       fruit [2]
#> # Groups:    fruit [2]
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2011 cherry     4
#> 2  2012 cherry     7
#> 3  2013 cherry    21
#> 4  2014 cherry    10
#> 5  2010 kiwi       6
#> 6  2011 kiwi       2
#> 7  2012 kiwi       9
#> 8  2013 kiwi       1

# leaves existing `NA` untouched
harvest[2, 3] <- NA
harvest %>%
  group_by_key() %>%
  fill_gaps(kilo = sum(kilo, na.rm = TRUE))
#> # A tsibble: 8 x 3 [1Y]
#> # Key:       fruit [2]
#> # Groups:    fruit [2]
#>    year fruit   kilo
#>   <dbl> <chr>  <int>
#> 1  2011 cherry     4
#> 2  2012 cherry    NA
#> 3  2013 cherry    14
#> 4  2014 cherry    10
#> 5  2010 kiwi       6
#> 6  2011 kiwi       2
#> 7  2012 kiwi       9
#> 8  2013 kiwi       1

# replace NA
pedestrian %>%
  group_by_key() %>%
  fill_gaps(Count = as.integer(median(Count)))
#> # A tsibble: 69,048 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#> # Groups:    Sensor [4]
#>    Sensor         Date_Time           Date        Time Count
#>    <chr>          <dttm>              <date>     <int> <int>
#>  1 Birrarung Marr 2015-01-01 00:00:00 2015-01-01     0  1630
#>  2 Birrarung Marr 2015-01-01 01:00:00 2015-01-01     1   826
#>  3 Birrarung Marr 2015-01-01 02:00:00 2015-01-01     2   567
#>  4 Birrarung Marr 2015-01-01 03:00:00 2015-01-01     3   264
#>  5 Birrarung Marr 2015-01-01 04:00:00 2015-01-01     4   139
#>  6 Birrarung Marr 2015-01-01 05:00:00 2015-01-01     5    77
#>  7 Birrarung Marr 2015-01-01 06:00:00 2015-01-01     6    44
#>  8 Birrarung Marr 2015-01-01 07:00:00 2015-01-01     7    56
#>  9 Birrarung Marr 2015-01-01 08:00:00 2015-01-01     8   113
#> 10 Birrarung Marr 2015-01-01 09:00:00 2015-01-01     9   166
#> # ℹ 69,038 more rows

if (!requireNamespace("tidyr", quietly = TRUE)) {
  stop("Please install the 'tidyr' package to run these following examples.")
}
# use fill() to fill `NA` by previous/next entry
pedestrian %>%
  group_by_key() %>%
  fill_gaps() %>%
  tidyr::fill(Count, .direction = "down")
#> # A tsibble: 69,048 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#> # Groups:    Sensor [4]
#>    Sensor         Date_Time           Date        Time Count
#>    <chr>          <dttm>              <date>     <int> <int>
#>  1 Birrarung Marr 2015-01-01 00:00:00 2015-01-01     0  1630
#>  2 Birrarung Marr 2015-01-01 01:00:00 2015-01-01     1   826
#>  3 Birrarung Marr 2015-01-01 02:00:00 2015-01-01     2   567
#>  4 Birrarung Marr 2015-01-01 03:00:00 2015-01-01     3   264
#>  5 Birrarung Marr 2015-01-01 04:00:00 2015-01-01     4   139
#>  6 Birrarung Marr 2015-01-01 05:00:00 2015-01-01     5    77
#>  7 Birrarung Marr 2015-01-01 06:00:00 2015-01-01     6    44
#>  8 Birrarung Marr 2015-01-01 07:00:00 2015-01-01     7    56
#>  9 Birrarung Marr 2015-01-01 08:00:00 2015-01-01     8   113
#> 10 Birrarung Marr 2015-01-01 09:00:00 2015-01-01     9   166
#> # ℹ 69,038 more rows
```
