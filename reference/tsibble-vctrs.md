# Internal vctrs methods

These methods are the extensions that allow tsibble objects to work with
vctrs.

## Usage

``` r
# S3 method for class 'tbl_ts'
vec_ptype2(x, y, ...)

# S3 method for class 'tbl_ts'
vec_cast(x, to, ...)

# S3 method for class 'yearmonth'
vec_cast(x, to, ...)

# S3 method for class 'yearmonth'
vec_ptype2(x, y, ...)

# S3 method for class 'yearmonth'
vec_arith(op, x, y, ...)

# S3 method for class 'yearmonth'
obj_print_data(x, ...)

# S3 method for class 'yearquarter'
vec_cast(x, to, ...)

# S3 method for class 'yearquarter'
vec_ptype2(x, y, ...)

# S3 method for class 'yearquarter'
vec_arith(op, x, y, ...)

# S3 method for class 'yearquarter'
obj_print_data(x, ...)

# S3 method for class 'yearweek'
vec_cast(x, to, ...)

# S3 method for class 'yearweek'
vec_ptype2(x, y, ...)

# S3 method for class 'yearweek'
vec_arith(op, x, y, ...)
```
