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


