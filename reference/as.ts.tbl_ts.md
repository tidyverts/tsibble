# Coerce a tsibble to a time series

**\[stable\]**

## Usage

``` r
# S3 method for class 'tbl_ts'
as.ts(x, value, frequency = NULL, fill = NA_real_, ...)
```

## Arguments

- x:

  A `tbl_ts` object.

- value:

  A measured variable of interest to be spread over columns, if multiple
  measures.

- frequency:

  A smart frequency with the default `NULL`. If set, the preferred
  frequency is passed to [`ts()`](https://rdrr.io/r/stats/ts.html).

- fill:

  A value to replace missing values.

- ...:

  Ignored for the function.

## Value

A `ts` object.

## Examples

``` r
# a monthly series
x1 <- as_tsibble(AirPassengers)
as.ts(x1)
#>      Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
#> 1949 112 118 132 129 121 135 148 148 136 119 104 118
#> 1950 115 126 141 135 125 149 170 170 158 133 114 140
#> 1951 145 150 178 163 172 178 199 199 184 162 146 166
#> 1952 171 180 193 181 183 218 230 242 209 191 172 194
#> 1953 196 196 236 235 229 243 264 272 237 211 180 201
#> 1954 204 188 235 227 234 264 302 293 259 229 203 229
#> 1955 242 233 267 269 270 315 364 347 312 274 237 278
#> 1956 284 277 317 313 318 374 413 405 355 306 271 306
#> 1957 315 301 356 348 355 422 465 467 404 347 305 336
#> 1958 340 318 362 348 363 435 491 505 404 359 310 337
#> 1959 360 342 406 396 420 472 548 559 463 407 362 405
#> 1960 417 391 419 461 472 535 622 606 508 461 390 432
```
