# tsibble 0.1.6.9000

This release (hopefully) marks the stability of a tsibble data object (`tbl_ts`). A `tbl_ts` contains the following components:

* `key`: single or multiple columns uniquely identify observational units over time. A key consisting of nested and crossed variables reflects the structure underlying the data. The programme itself takes care of the updates in the "key" when manipulating the data. The "key" differs from the grouping variables with respect to variables manipulated by users.
* `index`: a variable represent time. This together the "key" uniquely identifies each observation in the data table.
* `index2`: why do we need the second index? It means re-indexing **to** a variable, not the second index. It is identical to the `index` most time, but start deviating when using `index_by()`. `index_by()` works similarly to `group_by()`, but groups the index only. The dplyr verbs, like `filter()`, `mutate()`, operates on each time group of the data defined by `index_by()`. You may wonder why introducing a new function rather than using `group_by()` that users are most familiar with. It's because time is indispensable to a tsibble, `index_by()` provides a trace to understanding how the index changes. For this purpose, `group_by()` is just too general. For example, `index_by()` + `summarise()` aggregates data to less granular time period, leading to the update in index, which is nicely and intuitively handled now.
* `interval`: an `interval` class to save a list of time intervals. It computes the greatest common factor from the time difference of the `index` column, which should give a sensible interval for the almost all the cases, compared to minimal time distance. It also depends on the time representation. For example, if the data is monthly, the index is suggested to use a `yearmonth()` format instead of `Date`, as `Date` only gives the number of days not the number of months.
* `regular`: since a tsibble factors in the implicit missing cases, whether the data is regular or not cannot be determined. This relies on the user's specification.
* `ordered`: time-wise and rolling window functions assume data of temporal ordering. A tsibble will be sorted by its time index. If a key is explicitly declared, the key will be sorted first and followed by arranging time in ascending order. If it's not in time order, it broadcasts a warning.

## Breaking changes

* Deprecated `tsummarise()` and its scoped variants. It can be replaced by the combo `index_by()` + `summarise()` (#20). `tsummarise()` provides an unintuitive interface where the first argument keeps the same size of the index, but the remaining arguments reduces rows to a single one. Analogously, it does `group_by()` and then `summarise()`. The proposed `index_by()` solves the issue of index update.
* Renamed `inform_duplicates()` (defunct) to `find_duplicates()` to better reflect its functionality.
* `key_vars()` and `group_vars()` return a vector of characters instead of a list.
* `distinct.tbl_ts()` now returns a tibble instead of an error.
* No longer reexported `dplyr::do()` and `tidyr::fill()`, as they respect the input structure.
* Defunct `index_sum()`, and replaced by `index_valid()` to extend index type support.

## New functions

* `index_by()` groups time index, as the counterpart of `group_by()` in temporal context.
* A new S3 generic `count_gaps()` and `gaps()` counts time gaps (implicit missing observations in time).
* `yearweek()` creates and coerces to a year-week object. (#17)

## API changes

* `fill_na.tbl_ts()` gained a new argument of `.full = FALSE`. `.full = FALSE` (the default) inserts `NA` for each key within its time period, `TRUE` for the entire time span. This affects the results of `fill_na.tbl_ts()` as it only took `TRUE` into account previously. (#15)
* Renamed the `drop` argument to `.drop` in column-wise dplyr verbs.
* Dropped the `duplicated` argument in `pull_interval()`.
* `group_by.tbl_ts()` behaves exactly the same as `group_by.tbl_df` now. Grouping variables are temporary for data manipulation. Nested or crossed variables are not the type that `group_by()` thinks.

## Improvements

* Added overall time span to the `glimpse.tbl_ts()`.
* Slightly improved the speed of `fill_na()`.

## Bug fixes

* Fixed `transmute.tbl_ts()` for a univariate time series due to unregistered tidyselect helpers. (#9).
* Fixed bug in `select.tbl_ts()` and `rename.tbl_ts()` for not preserving grouped variables (#12).
* Fixed bug in `select.tbl_ts()` and `rename.tbl_ts()` for renaming grouped variables.

## Internal changes

* A `tbl_ts` gains a new attribute `index2`, which is a candidate of new index (symbol) used by `index_by()`.
* The time interval is obtained through the greatest common factor of positive time differences. This covers broader cases than the minimal value.
* `attr(grouped_ts, "vars")` stores characters instead of names, same as `attr(grouped_df, "vars")`.

# tsibble 0.1.5

This release introduces major changes into the underlying `tbl_ts` object:

* Dropped the attribute "key_indices" from a `tbl_ts` class to reduce the object size, and computed on the fly when printing.
* Gained a new attribute "ordered" to identify if it is arranged by key and index in ascending order. If not, broadcast a warning. The warning likely occurs to `arrange()` and `slice()` functions.
* The "index" attribute in a `tbl_ts` object is a symbol now instead of a quosure.
* The "key" attribute in a `tbl_ts` object is an unnamed list of symbols.

## New functions

* "key" helpers:
  * `key_update()` to change/update the keys for a given tsibble.
  + `unkey()` as an S3 method for a tsibble of key size < 2.
  + `key_indices()` as an S3 method to extract key indices.
* `split_by()` to split a tsibble into a list of data by unquoted variables.
* `build_tsibble()` allows users to gain more control over a tsibble construction.
* Added `as_tsibble.msts()` for multiple seasonality time series defined in the forecast package.

## Bug fixes

* Fixed `as_tsibble.ts()` for daily time series (when frequency = 7).
* `group_by.tbl_ts()` does not accept named expressions.

## Internal changes

* No longer throw an error when grouping the index.
* An interval of regularly spaced tsibble is (re)computed when creating the tsibble and performing the row-wise operations (like `filter()` and `slice()`). This avoids unnecessary re-computation for many function calls.

# tsibble 0.1.3

## New functions

* Added the scoped variants for `tsummarise()` including `tsummarise_all()`, `tsummarise_if()`, `tsummarise_at()`.

## API changes

* The windowed functions, including `slide()`, `tile()`, `stretch()`, are no longer defined as S3 methods. Several new variants have been introduced for the purpose of type stability, like `slide_lst()` (a list), `slide_dfr()` (a row-binding data frame), `slide_dfc()` (a column-binding data frame).
* The `index` variable must sit in the first name-value pair in `tsummarise()` instead of any position in the call.
* `transmute.tbl_ts()` keeps the newly created variables along with index and keys, instead of throwing an error before.
* Depends on purrr (>= 0.2.3)

## Bug fixes

* Fixed the error message in `glimpse.tbl_ts()`
* Fixed `format.key()` for nesting crossed with another nesting.

# tsibble 0.1.2

This release marks the complete support of dplyr key verbs.

## Reexported functions

* `tidyr::fill()` fills `NA` backward or forward in tsibble.
* Implement `tbl_ts` support for `dplyr::*_join()`.
* No `tbl_ts` support for `dplyr::transmute()` and `dplyr::distinct()` and return an error. 

## New functions

* `inform_duplicates()` informs which row has duplicated elements of key and index variables.

## Bug fixes

* Fix bug in `summarise.tbl_ts()` and `tsummarise.tbl_ts()`, when calling functions with no parameters like `dplyr::n()`.
* In `summarise.tbl_ts()` and `tsummarise.tbl_ts()`, one grouping level should be dropped for the consistency with `dplyr::summarise()` for a grouped `tbl_ts`.
* Fix incorrect group and key indices.
* `NULL` and `tbl_ts` are supported in `as_tsibble()`. An empty tsibble is not allowed.
* `group_by.tbl_ts(.data, ..., add = TRUE)` works as expected now.

## Internal changes

* Better handling `grouped_ts` and `grouped_df`.
* More informative error messages.

# tsibble 0.1.0

* Initial release on CRAN.

# tsibble 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Pre-release on Github


