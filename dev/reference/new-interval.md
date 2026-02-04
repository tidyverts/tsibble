# Interval constructor for a tsibble

**\[stable\]**

- `new_interval()` creates an interval object.

- `gcd_interval()` computes the greatest common divisor for the
  difference of numerics.

- `is_regular_interval()` checks if the interval is regular.

## Usage

``` r
new_interval(..., .regular = TRUE, .others = list())

is_regular_interval(x)

gcd_interval(x)
```

## Arguments

- ...:

  A set of name-value pairs to specify default interval units: "year",
  "quarter", "month", "week", "day", "hour", "minute", "second",
  "millisecond", "microsecond", "nanosecond", "unit".

- .regular:

  Logical. `FALSE` gives an irregular interval, and will ignore the
  `...` argument.

- .others:

  A list name-value pairs that are not included in the `...`, to allow
  custom interval.

- x:

  An interval.

## Value

an "interval" class

## Examples

``` r
(x <- new_interval(hour = 1, minute = 30))
#> <interval[1]>
#> [1] 1h 30m
(y <- new_interval(.regular = FALSE)) # irregular interval
#> <interval[1]>
#> [1] !
new_interval() # unknown interval
#> <interval[1]>
#> [1] ?
new_interval(.others = list(semester = 1)) # custom interval
#> <interval[1]>
#> [1] 1semester
is_regular_interval(x)
#> [1] TRUE
is_regular_interval(y)
#> [1] FALSE
gcd_interval(c(1, 3, 5, 6))
#> [1] 1
```
