# tsibble 0.6.2.9000 (to be released as v0.7.0)

Following the changes in dplyr grouped data frames, tsibble allows for empty key values and disregards the lazily stored key. All operations now recalculate the keying structure.

## New features

* `scan_gaps()` & `count_gaps()` gained a new argument `.common = FALSE` to specify common time gaps shared by each series.

## Bug fixes

* Fixed `rbind()` for dropping custom index class. (#78)
* Fixed `count_gaps()` for dropping custom index class.
* `count_gaps()` now only summarises keys with gaps instead of all the keys.

## Misc

* Defunct `find_duplicates()`.
* Deprecated `fill_na()` in favour of `fill_gaps()`.
* Defunct argument `.drop` in column-wise dplyr verbs.

# tsibble 0.6.2

## New features

* `scan_gaps()` joins the family of implicit missing values handlers.
* The rolling window family gains parallel processing capabilities, prefixed by `future_`. It requires the **furrr** package to be installed. (#66)

## Bug fixes

* Regression bug (since tsibble v0.6.0) for subsetting tsibble using `[.tbl_ts`. (#76)
* Issue a warning when a system time zone is "Europe/London" for `time_in()` and `filter_index()`.

## Misc

* Defunct `split_by()` and `case_na()`.
* Deprecated `find_duplicates()`.
* Continue with soft-deprecation of `fill_na()`.

# tsibble 0.6.1

* Fixed check errors on Fedora.
* If `.data` is a complete tsibble, `fill_gaps()` gives a warning instead of an error when name-value pairs are supplied.
* `filter_index()` works for a grouped tsibble.

# tsibble 0.6.0

This release simplifies the "key" structure. The nesting and crossing definition has been removed from the "key" specification. One or more variables forming the "key", are required to identify observational units over time, but no longer assume the relationship between these variables. The nesting and crossing structure will be dealt with visualisation and forecasting reconciliation in downstream packages.

## Breaking changes

* `count_gaps.tbl_ts()` returns a tibble containing gaps for each key value rather than an overall gap, which is consistent with the rest of tsibble methods. And all output column names that are not supplied by users gain a prefixed ".".
* `time_unit()` accepts `interval` input instead of time vectors to avoid overheads, also marked as internal function.
* Separate partial sliding from `slider()` and `pslider()` as new functions `partial_slider()` and `partial_pslider()`. Argument `.partial` is removed from `slider()` and `pslider()` to feature a simpler interface.
* Removed argument `group` from `build_tsibble()`. In order to construct a grouped tsibble, `x` requires a grouped df.

## New features

* Added S3 generic `has_gaps()` to quickly check if there are implicit time gaps for each key in a tsibble.
* Added S3 generic `new_data()` to produce the future of a tsibble.
* A shorthand `filter_index()` to filter time window for a tsibble.
* New S3 generic `time_in()` to check if time falls in the ranges in compact expression, with no need for time zone specification.
* An empty tsibble is now valid and will not raise an error, which makes it easier to programme with.
* New vignettes on handling implicit missingness and FAQ.
* `new_tsibble()` creates a subclass of a tsibble.
* Renamed `fill_na()` to `fill_gaps()`, for more expressive function name and consistency to `has_gaps()` and `count_gaps()`. Soft-deprecated `fill_na()`. (#71)
* Added `is_duplicated()`, `are_duplicated()` and `duplicates()`.
* If `POSIXct`, time zone will be displayed in the header via `print()`.
* Added index support for ordered factors.

## Bug fixes

* Fixed unexpected error message for `holiday_aus()` that requires package "timeDate".
* Fixed `summarise.tbl_ts()`, `select.tbl_ts()` & `fill_na.tbl_ts()` scoping issue (#67).
* `slice.tbl_ts()` correctly handles logical `NA`.
* `fill_na()` will only replace implicit time gaps by values and functions, and leave originally explicit `NA` intact. 
* `tidyr::fill()` gained support for class "grouped_ts", and it is re-exported again. (#73)

## Misc

* Soft-deprecated `fill_na()`, in favour of `fill_gaps()`.
* Soft-deprecated `find_duplicates()`, in favour of `are_duplicated()`.
* Deprecated `case_na()`, and will be defunct in next release.
* Deprecated `split_by()`, which is under development as S3 generic in **dplyr**.
* Soft-deprecated `as.tsibble()`, following `as.tibble()` in **tibble**.
* Deprecated `.drop` argument in column-wise verbs, and suggested to use `as_tibble()`.
* If `select()` doesn't select index, it will inform users and automatically select it.

# tsibble 0.5.3

## New features

* Added `append_row()` for easily appending new observations to a tsibble. (#59)
* Nesting structure can be specified using `/`, consistent with `lm`. (#64)

## Bug fixes

* Fixed warning in `fill_na()` for multiple replacements when using with `group_by()`, introduced in v0.5.1.
* `as_tsibble.grouped_df()` respected its existing groups and removed argument `group`. (#60)
* Preserve the ordering of key specification in `select.tbl_ts()`. (#63)

## Misc

* Soft-deprecated `case_na()` & `split_by()`.

# tsibble 0.5.2

## Bug fixes

* `unnest.lst_ts` respects the ordering of "key" values. (#56)
* `split_by()` and `nest.tbl_ts()` respect the appearance ordering of input variables. (#57)
* `group_indices.tbl_ts()` and `key_indices()` return consistent formats as its generic.
* Argument `key` no longer accepted character.

## Improvements

* Slightly faster `nest.tbl_ts()`.
* `index_by()` gives more informative error when LHS is named as index.

## Changes catering for the updates in `tibble`

* No longer reexported `as.tibble()`.
* Reexported `tibble()`.

# tsibble 0.5.1

## New features

* `tile()` gained a new argument `.bind = FALSE`.
* Vectorised arithmetic operators (`+` & `-`) for yearweek, yearmonth, and yearquarter.
* `new_interval()` creates an "interval" object with the specified values.

## Improvements

* Faster performance in `fill_na()` for replacing values when `group_by()`.
* Speed improvement when subsetting yearweek, yearmonth and yearquarter using `[`.

## Bug fixes

* Fixed key updating via `group_by()` + `summarise()`. (#47)
* Respected the ordering of input variables while creating the key.
* Fixed "attempt to select less than one element in integerOneIndex" error message in `unnest.lst_ts()`.
* Fixed incorrect interval when doing join operations for custom index class. (#52)
* Avoided warnings when character input for key and value in `gather.tbl_ts()`. (#54)

## Misc

* `slide()` & `stretch()` use the same coercion rules as `dplyr::combine()` now, if `.bind = TRUE`.
* Avoid strong dependency on `pillar`.
* Setting row names on a tsibble is deprecated, which is consistent with `tibble`. 

# tsibble 0.5.0

This release introduced the breaking changes to the "interval" class to make tsibble better support finer time resolution (e.g. millisecond, microsecond, and nanosecond). The "interval" format changes from upper case to short hand. To support new time index class, only `index_valid()` and `pull_interval()` need to be defined now.

## New features

* Added "nanotime" support for nanoseconds.
* Added scoped variant `group_by_key()` to easily group the key variables.
* `slide()` gained a new argument `.align = "right"` to align at "right", "center", or "left". If window size is even for center alignment, either "center-right" or "center-left" is needed.
* Defined arithmetic operators (`+` & `-`) for yearweek, yearmonth, and yearquarter.
* `slide()` and `stretch()` gained a new argument `.bind = FALSE`.
* A new vignette on window functions.

## Improvements

* Speed improvement for internals when it's a known valid tsibble. (#43)
* Better support "millisecond" and "microsecond".

## Internal changes

* Replaced `NA` or `NULL` with `0` in the "interval" class to make the representation simpler.
* The `interval` class has new slots of "millisecond", "microsecond", "nanosecond".
* `time_unit()` is a function instead of S3 generic, and made index extension a bit easier.

## Bug fixes

* Fixed warning in `format.yearweek()`.
* Fixed `group_by.lst_ts()` for dropping the grouping information.
* Fixed `stretch2()` only applying `.f` to one input.
* Fixed NSE in `as_tsibble.grouped_df()` for groups. (#44)
* Fixed bug in `.fill = NULL` for `slide()`.

## Misc

* Moved package "timeDate" from Imports to Suggests.
* Added "anytime" to Imports for better parsing characters.

# tsibble 0.4.0

## Breaking changes

* Restructured windowed functions to follow the `purrr` style exactly (#35): 
  + `slide()`, `tile()`, `stretch()` return lists only instead of numerics before.
  + added `slide2()`, `pslide()` to map over multiple inputs simultaneously. 
  + added other type-stable variants such as `slide_dbl()`, `slide_int()`, `slide_chr()`, `slide_lgl()`.
  * A negative window size results in a backward moving direction.
  * `slide()` gained a new argument `.partial` to support partial sliding.
* Renamed `x` to `.x` in `slider()`, `tiler()`, `stretcher()`.
* `pslider()`, `ptiler()`, `pstretcher()` support multiple inputs now, and split them in parallel.

## New features

* New `holiday_aus()` for Australian national and state-based public holiday.
* Defined `diff()` for year-week, year-month, and year-quarter.
* `yearweek()`, `yearmonth()`, `yearquarter()` supported for character.
* Added `slide2()`, `pslide()`, `tile2()`, `ptile()`, `stretch2()`, `pstretch()` to slide over multiple inputs simultaneously (#33).
* New S3 generics `units_since()` for index classes.
* New `is_53weeks()` for determine if the year has 53 ISO weeks.
* New S3 generics `key_sum()` for extending tsibble.

## Improvements

* `as_tsibble.ts()` removed the `tsp` attribute from the `value` column.
* Dropped the "lst_col" attribute from `lst_ts` (#25).
* More informative error message when a data frame is passed to `tsibble()`.
* More informative error message for `fill_na()` and `count_gaps` when a tsibble of unknown interval.
* `as_tsibble.tbl_ts()` & `as_tsibble.grouped_ts()` now return self (#34).
* `id()` is used in the tsibble context (e.g. `as_tsibble()`, `tsibble()`, `build_tsibble()`) regardless of the conflicts with dplyr or plyr, to avoid frustrating message (#36).
* `select.tbl_ts()` now preserved index.

## Bug fixes

* Fixed bug in `as.ts.tbl_ts()` for ignoring the `value` argument when the key is empty.
* Fixed bug in `[.tbl_ts()` when subsetting columns by characters (#30).
* Fixed bug in `fill_na.tbl_ts()` dropping custom index class (#32).
* Fixed bug in `format.yearweek()` due to the boundary issue (#27).
* If a column passed as `index` contains `NA`, abort.

## Misc

* Suggested on `nycflights13 >= 1.0.0`.

# tsibble 0.3.0

The **tsibble** package has a hexagon logo now! Thanks Mitch (@mitchelloharawild).

## New functions

* New `difference()` computes lagged differences of a numeric vector. It returns a vector of the same length as the input with `NA` padded. It works with `mutate()`.
* Added the support of `gather()`/`spread()`, `nest()`/`unnest()` to `tbl_ts`.

## Defunct

* retired `tsummarise()`.

## Internal changes

* If `index2` deviates from `index` (using `index_by()`), the `index2` will be part of grouping variables.

# tsibble 0.2.0

This release (hopefully) marks the stability of a tsibble data object (`tbl_ts`). A `tbl_ts` contains the following components:

* `key`: single or multiple columns uniquely identify observational units over time. A key consisting of nested and crossed variables reflects the structure underlying the data. The programme itself takes care of the updates in the "key" when manipulating the data. The "key" differs from the grouping variables with respect to variables manipulated by users.
* `index`: a variable represents time. This together the "key" uniquely identifies each observation in the data table.
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


