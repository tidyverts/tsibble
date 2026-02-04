# Australian national and state-based public holiday

Australian national and state-based public holiday

## Usage

``` r
holiday_aus(year, state = "national")
```

## Arguments

- year:

  A vector of integer(s) indicating year(s).

- state:

  A state in Australia including "ACT", "NSW", "NT", "QLD", "SA", "TAS",
  "VIC", "WA", as well as "national".

## Value

A tibble consisting of `holiday` labels and their associated dates in
the year(s).

## Details

Not documented public holidays:

- AFL public holidays for Victoria

- Queen's Birthday for Western Australia

- Royal Queensland Show for Queensland, which is for Brisbane only

This function requires "timeDate" to be installed.

## Examples

``` r
holiday_aus(2016, state = "VIC")
#> # A tibble: 12 × 2
#>    holiday          date      
#>    <chr>            <date>    
#>  1 New Year's Day   2016-01-01
#>  2 Australia Day    2016-01-26
#>  3 Labour Day       2016-03-14
#>  4 Good Friday      2016-03-25
#>  5 Easter Saturday  2016-03-26
#>  6 Easter Sunday    2016-03-27
#>  7 Easter Monday    2016-03-28
#>  8 ANZAC Day        2016-04-25
#>  9 Queen's Birthday 2016-06-13
#> 10 Melbourne Cup    2016-11-01
#> 11 Boxing Day       2016-12-26
#> 12 Christmas Day    2016-12-27
holiday_aus(2013:2016, state = "ACT")
#> # A tibble: 44 × 2
#>    holiday          date      
#>    <chr>            <date>    
#>  1 New Year's Day   2013-01-01
#>  2 Australia Day    2013-01-28
#>  3 Canberra Day     2013-03-11
#>  4 Good Friday      2013-03-29
#>  5 Easter Saturday  2013-03-30
#>  6 Easter Sunday    2013-03-31
#>  7 Easter Monday    2013-04-01
#>  8 ANZAC Day        2013-04-25
#>  9 Queen's Birthday 2013-06-10
#> 10 Christmas Day    2013-12-25
#> # ℹ 34 more rows
```
