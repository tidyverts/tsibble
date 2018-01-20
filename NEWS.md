# tsibble 0.1.0.9999

## New functions

* Added the `tidyr::fill()` support for tsibble to fill `NA` backward or forward.
* Added the `dplyr::*_join()` join family support for tsibble.

## Bug fixes

* Fixed the bug in `summarise.tbl_ts()` and `tsummarise.tbl_ts()`, when calling functions with no parameters like `dplyr::n()`. (@mitchelloharawild)
* One grouping level should be dropped for the consistency with `dplyr::summarise()` for a grouped data frame. (@mitchelloharawild)
* Fixed incorrect group and key indices.

# tsibble 0.1.0

* Initial release on CRAN.

# tsibble 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* Pre-release on Github


