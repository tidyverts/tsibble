# Package index

## Overview

Describes the key components underlying a **tsibble**, or `tbl_ts`:
index, key, interval.

- [`tsibble-package`](https://tsibble.tidyverts.org/dev/reference/tsibble-package.md)
  : tsibble: tidy temporal data frames and tools

## Create/coerce and append to a tsibble

[`tsibble()`](https://tsibble.tidyverts.org/dev/reference/tsibble.md)
creates a `tbl_ts`;
[`as_tsibble()`](https://tsibble.tidyverts.org/dev/reference/as-tsibble.md)
coerces other objects to `tbl_ts`.

- [`tsibble()`](https://tsibble.tidyverts.org/dev/reference/tsibble.md)
  : Create a tsibble object
- [`as_tsibble()`](https://tsibble.tidyverts.org/dev/reference/as-tsibble.md)
  **\[stable\]** : Coerce to a tsibble object
- [`is_tsibble()`](https://tsibble.tidyverts.org/dev/reference/is-tsibble.md)
  [`is_grouped_ts()`](https://tsibble.tidyverts.org/dev/reference/is-tsibble.md)
  **\[stable\]** : If the object is a tsibble
- [`update_tsibble()`](https://tsibble.tidyverts.org/dev/reference/update_tsibble.md)
  : Update key and index for a tsibble
- [`new_data()`](https://tsibble.tidyverts.org/dev/reference/new-data.md)
  [`append_row()`](https://tsibble.tidyverts.org/dev/reference/new-data.md)
  **\[stable\]** : New tsibble data and append new observations to a
  tsibble
- [`is_duplicated()`](https://tsibble.tidyverts.org/dev/reference/duplicates.md)
  [`are_duplicated()`](https://tsibble.tidyverts.org/dev/reference/duplicates.md)
  [`duplicates()`](https://tsibble.tidyverts.org/dev/reference/duplicates.md)
  **\[stable\]** : Test duplicated observations determined by key and
  index variables

## Tsibble verbs

Verbs that manipulate data in time-based context. Inspect implicit time
gaps with
[`has_gaps()`](https://tsibble.tidyverts.org/dev/reference/has_gaps.md),
[`scan_gaps()`](https://tsibble.tidyverts.org/dev/reference/scan_gaps.md),
[`count_gaps()`](https://tsibble.tidyverts.org/dev/reference/count_gaps.md)
and
[`fill_gaps()`](https://tsibble.tidyverts.org/dev/reference/fill_gaps.md).
[`filter_index()`](https://tsibble.tidyverts.org/dev/reference/filter_index.md)
is a shorthand for filtering time index.
[`index_by()`](https://tsibble.tidyverts.org/dev/reference/index-by.md)
and
[`group_by_key()`](https://tsibble.tidyverts.org/dev/reference/group_by_key.md)
create a grouped tsibble, or `grouped_ts`, but how the data looks
remains unchanged. Both requires to work with other `tidyverse` verbs.

- [`has_gaps()`](https://tsibble.tidyverts.org/dev/reference/has_gaps.md)
  : Does a tsibble have implicit gaps in time?

- [`scan_gaps()`](https://tsibble.tidyverts.org/dev/reference/scan_gaps.md)
  : Scan a tsibble for implicit missing observations

- [`count_gaps()`](https://tsibble.tidyverts.org/dev/reference/count_gaps.md)
  : Count implicit gaps

- [`fill_gaps()`](https://tsibble.tidyverts.org/dev/reference/fill_gaps.md)
  **\[stable\]** : Turn implicit missing values into explicit missing
  values

- [`index_by()`](https://tsibble.tidyverts.org/dev/reference/index-by.md)
  **\[stable\]** :

  Group by time index and collapse with
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)

- [`group_by_key()`](https://tsibble.tidyverts.org/dev/reference/group_by_key.md)
  **\[stable\]** : Group by key variables

- [`filter_index()`](https://tsibble.tidyverts.org/dev/reference/filter_index.md)
  : A shorthand for filtering time index for a tsibble

## Tidyverse methods

Dplyr and tidyr verbs that support manipulating and reshaping `tbl_ts`
in time-based context.

- [`tsibble-tidyverse`](https://tsibble.tidyverts.org/dev/reference/tsibble-tidyverse.md)
  : Tidyverse methods for tsibble

## Vector operations

- [`difference()`](https://tsibble.tidyverts.org/dev/reference/difference.md)
  **\[stable\]** : Lagged differences

## Index classes

New S3 classes to represent year-week, year-month, and year-quarter.

- [`yearmonth()`](https://tsibble.tidyverts.org/dev/reference/year-month.md)
  [`make_yearmonth()`](https://tsibble.tidyverts.org/dev/reference/year-month.md)
  [`is_yearmonth()`](https://tsibble.tidyverts.org/dev/reference/year-month.md)
  **\[stable\]** : Represent year-month
- [`yearquarter()`](https://tsibble.tidyverts.org/dev/reference/year-quarter.md)
  [`make_yearquarter()`](https://tsibble.tidyverts.org/dev/reference/year-quarter.md)
  [`is_yearquarter()`](https://tsibble.tidyverts.org/dev/reference/year-quarter.md)
  [`fiscal_year()`](https://tsibble.tidyverts.org/dev/reference/year-quarter.md)
  **\[stable\]** : Represent year-quarter
- [`yearweek()`](https://tsibble.tidyverts.org/dev/reference/year-week.md)
  [`make_yearweek()`](https://tsibble.tidyverts.org/dev/reference/year-week.md)
  [`is_yearweek()`](https://tsibble.tidyverts.org/dev/reference/year-week.md)
  [`is_53weeks()`](https://tsibble.tidyverts.org/dev/reference/year-week.md)
  **\[stable\]** : Represent year-week based on the ISO 8601 standard
  (with flexible start day)
- [`time_in()`](https://tsibble.tidyverts.org/dev/reference/time_in.md)
  : If time falls in the ranges using compact expressions
- [`guess_frequency()`](https://tsibble.tidyverts.org/dev/reference/guess_frequency.md)
  **\[stable\]** : Guess a time frequency from other index objects
- [`scale_x_yearquarter()`](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
  [`scale_y_yearquarter()`](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
  [`scale_x_yearmonth()`](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
  [`scale_y_yearmonth()`](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
  [`scale_x_yearweek()`](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
  [`scale_y_yearweek()`](https://tsibble.tidyverts.org/dev/reference/tsibble-scales.md)
  : tsibble scales for ggplot2

## Calendar

- [`holiday_aus()`](https://tsibble.tidyverts.org/dev/reference/holiday_aus.md)
  : Australian national and state-based public holiday

## Metadata

- [`key()`](https://tsibble.tidyverts.org/dev/reference/key.md)
  [`key_vars()`](https://tsibble.tidyverts.org/dev/reference/key.md) :

  or other tidyselect-compatible functions,
  [`key_vars()`](https://tsibble.tidyverts.org/dev/reference/key.md)
  acts as a selection helper that automatically selects all key columns
  from the tsibble.

- [`key_data()`](https://tsibble.tidyverts.org/dev/reference/key-data.md)
  [`key_rows()`](https://tsibble.tidyverts.org/dev/reference/key-data.md)
  [`key_size()`](https://tsibble.tidyverts.org/dev/reference/key-data.md)
  [`n_keys()`](https://tsibble.tidyverts.org/dev/reference/key-data.md)
  : Key metadata

- [`index()`](https://tsibble.tidyverts.org/dev/reference/index-rd.md)
  [`index_var()`](https://tsibble.tidyverts.org/dev/reference/index-rd.md)
  [`index2()`](https://tsibble.tidyverts.org/dev/reference/index-rd.md)
  [`index2_var()`](https://tsibble.tidyverts.org/dev/reference/index-rd.md)
  : Return index variable from a tsibble

- [`measures()`](https://tsibble.tidyverts.org/dev/reference/measured-vars.md)
  [`measured_vars()`](https://tsibble.tidyverts.org/dev/reference/measured-vars.md)
  : Return measured variables

- [`interval()`](https://tsibble.tidyverts.org/dev/reference/regular.md)
  [`is_regular()`](https://tsibble.tidyverts.org/dev/reference/regular.md)
  [`is_ordered()`](https://tsibble.tidyverts.org/dev/reference/regular.md)
  : Meta-information of a tsibble

## Extend tsibble

Add **tsibble** support for custom index classes with
[`index_valid()`](https://tsibble.tidyverts.org/dev/reference/index_valid.md)
&
[`interval_pull()`](https://tsibble.tidyverts.org/dev/reference/interval-pull.md).
[`build_tsibble()`](https://tsibble.tidyverts.org/dev/reference/build_tsibble.md)
provides low-level construction for tsibble. Create a subclass of the
tsibble with
[`new_tsibble()`](https://tsibble.tidyverts.org/dev/reference/new_tsibble.md).

- [`index_valid()`](https://tsibble.tidyverts.org/dev/reference/index_valid.md)
  **\[stable\]** : Add custom index support for a tsibble
- [`interval_pull()`](https://tsibble.tidyverts.org/dev/reference/interval-pull.md)
  **\[stable\]** : Pull time interval from a vector
- [`new_interval()`](https://tsibble.tidyverts.org/dev/reference/new-interval.md)
  [`is_regular_interval()`](https://tsibble.tidyverts.org/dev/reference/new-interval.md)
  [`gcd_interval()`](https://tsibble.tidyverts.org/dev/reference/new-interval.md)
  **\[stable\]** : Interval constructor for a tsibble
- [`new_tsibble()`](https://tsibble.tidyverts.org/dev/reference/new_tsibble.md)
  : Create a subclass of a tsibble
- [`build_tsibble()`](https://tsibble.tidyverts.org/dev/reference/build_tsibble.md)
  : Low-level constructor for a tsibble object

## Coerce to other objects

- [`as.ts(`*`<tbl_ts>`*`)`](https://tsibble.tidyverts.org/dev/reference/as.ts.tbl_ts.md)
  **\[stable\]** : Coerce a tsibble to a time series
- [`as_tibble(`*`<tbl_ts>`*`)`](https://tsibble.tidyverts.org/dev/reference/as-tibble.md)
  : Coerce to a tibble or data frame

## Data

- [`pedestrian`](https://tsibble.tidyverts.org/dev/reference/pedestrian.md)
  : Pedestrian counts in the city of Melbourne
- [`tourism`](https://tsibble.tidyverts.org/dev/reference/tourism.md) :
  Australian domestic overnight trips
