# tsibble scales for ggplot2

Defines ggplot2 scales for tsibble custom index:
[yearweek](https://tsibble.tidyverts.org/reference/year-week.md),
[yearmonth](https://tsibble.tidyverts.org/reference/year-month.md), and
[yearquarter](https://tsibble.tidyverts.org/reference/year-quarter.md).

## Usage

``` r
scale_x_yearquarter(...)

scale_y_yearquarter(...)

scale_x_yearmonth(...)

scale_y_yearmonth(...)

scale_x_yearweek(...)

scale_y_yearweek(...)
```

## Arguments

- ...:

  Arguments passed to
  [`ggplot2::scale_x_date()`](https://ggplot2.tidyverse.org/reference/scale_date.html).

## Value

A ggproto object inheriting from `Scale`
