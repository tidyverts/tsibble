# New tsibble data and append new observations to a tsibble

**\[stable\]**

`append_row()`: add new rows to the start/end of a tsibble by filling a
key-index pair and `NA` for measured variables.

`append_case()` is an alias of `append_row()`.

## Usage

``` r
new_data(.data, n = 1L, ...)

# S3 method for class 'tbl_ts'
new_data(.data, n = 1L, keep_all = FALSE, ...)

append_row(.data, n = 1L, ...)
```

## Arguments

- .data:

  A `tbl_ts`.

- n:

  An integer indicates the number of key-index pair to append. If

  - `n > 0`, future observations

  - `n < 0`, past observations

- ...:

  Passed to individual S3 method.

- keep_all:

  If `TRUE` keep all the measured variables as well as index and key,
  otherwise only index and key.

## Examples

``` r
new_data(pedestrian)
#> # A tsibble: 4 x 2 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>   Sensor                        Date_Time          
#>   <chr>                         <dttm>             
#> 1 Birrarung Marr                2017-01-01 00:00:00
#> 2 Bourke Street Mall (North)    2017-01-01 00:00:00
#> 3 QV Market-Elizabeth St (West) 2017-01-01 00:00:00
#> 4 Southern Cross Station        2017-01-01 00:00:00
new_data(pedestrian, keep_all = TRUE)
#> # A tsibble: 4 x 5 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>   Sensor                        Date_Time           Date    Time Count
#>   <chr>                         <dttm>              <date> <int> <int>
#> 1 Birrarung Marr                2017-01-01 00:00:00 NA        NA    NA
#> 2 Bourke Street Mall (North)    2017-01-01 00:00:00 NA        NA    NA
#> 3 QV Market-Elizabeth St (West) 2017-01-01 00:00:00 NA        NA    NA
#> 4 Southern Cross Station        2017-01-01 00:00:00 NA        NA    NA
new_data(pedestrian, n = 3)
#> # A tsibble: 12 x 2 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>    Sensor                        Date_Time          
#>    <chr>                         <dttm>             
#>  1 Birrarung Marr                2017-01-01 00:00:00
#>  2 Birrarung Marr                2017-01-01 01:00:00
#>  3 Birrarung Marr                2017-01-01 02:00:00
#>  4 Bourke Street Mall (North)    2017-01-01 00:00:00
#>  5 Bourke Street Mall (North)    2017-01-01 01:00:00
#>  6 Bourke Street Mall (North)    2017-01-01 02:00:00
#>  7 QV Market-Elizabeth St (West) 2017-01-01 00:00:00
#>  8 QV Market-Elizabeth St (West) 2017-01-01 01:00:00
#>  9 QV Market-Elizabeth St (West) 2017-01-01 02:00:00
#> 10 Southern Cross Station        2017-01-01 00:00:00
#> 11 Southern Cross Station        2017-01-01 01:00:00
#> 12 Southern Cross Station        2017-01-01 02:00:00
new_data(pedestrian, n = -2)
#> # A tsibble: 8 x 2 [1h] <Australia/Melbourne>
#> # Key:       Sensor [4]
#>   Sensor                        Date_Time          
#>   <chr>                         <dttm>             
#> 1 Birrarung Marr                2014-12-31 22:00:00
#> 2 Birrarung Marr                2014-12-31 23:00:00
#> 3 Bourke Street Mall (North)    2015-02-16 22:00:00
#> 4 Bourke Street Mall (North)    2015-02-16 23:00:00
#> 5 QV Market-Elizabeth St (West) 2014-12-31 22:00:00
#> 6 QV Market-Elizabeth St (West) 2014-12-31 23:00:00
#> 7 Southern Cross Station        2014-12-31 22:00:00
#> 8 Southern Cross Station        2014-12-31 23:00:00

tsbl <- tsibble(
  date = rep(as.Date("2017-01-01") + 0:2, each = 2),
  group = rep(letters[1:2], 3),
  value = rnorm(6),
  key = group
)
#> Using `date` as index variable.
append_row(tsbl)
#> # A tsibble: 8 x 3 [1D]
#> # Key:       group [2]
#>   date       group  value
#>   <date>     <chr>  <dbl>
#> 1 2017-01-01 a      0.115
#> 2 2017-01-02 a     -1.41 
#> 3 2017-01-03 a     -1.24 
#> 4 2017-01-04 a     NA    
#> 5 2017-01-01 b     -1.77 
#> 6 2017-01-02 b      0.709
#> 7 2017-01-03 b     -0.368
#> 8 2017-01-04 b     NA    
append_row(tsbl, n = 2)
#> # A tsibble: 10 x 3 [1D]
#> # Key:       group [2]
#>    date       group  value
#>    <date>     <chr>  <dbl>
#>  1 2017-01-01 a      0.115
#>  2 2017-01-02 a     -1.41 
#>  3 2017-01-03 a     -1.24 
#>  4 2017-01-04 a     NA    
#>  5 2017-01-05 a     NA    
#>  6 2017-01-01 b     -1.77 
#>  7 2017-01-02 b      0.709
#>  8 2017-01-03 b     -0.368
#>  9 2017-01-04 b     NA    
#> 10 2017-01-05 b     NA    
append_row(tsbl, n = -2)
#> # A tsibble: 10 x 3 [1D]
#> # Key:       group [2]
#>    date       group  value
#>    <date>     <chr>  <dbl>
#>  1 2016-12-30 a     NA    
#>  2 2016-12-31 a     NA    
#>  3 2017-01-01 a      0.115
#>  4 2017-01-02 a     -1.41 
#>  5 2017-01-03 a     -1.24 
#>  6 2016-12-30 b     NA    
#>  7 2016-12-31 b     NA    
#>  8 2017-01-01 b     -1.77 
#>  9 2017-01-02 b      0.709
#> 10 2017-01-03 b     -0.368
```
