# fable

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/fable
* URL: https://fable.tidyverts.org
* BugReports: https://github.com/tidyverts/fable/issues
* Date/Publication: 2019-09-23 15:10:06 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"fable")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
    > tsibbledata::global_economy %>% 
    +   filter(Country == "Australia") %>%
    +   model(ARIMA(log(GDP) ~ Population))
    Error: Detecting a corrupt tsibble object, and please reconstruct with `as_tsibble()`.
    Backtrace:
    [90m     [39m█
    [90m  1. [39m└─`%>%`(...)
    [90m  2. [39m  ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
    [90m  3. [39m  └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
    [90m  4. [39m    └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
    [90m  5. [39m      └─`_fseq`(`_lhs`)
    [90m  6. [39m        └─magrittr::freduce(value, `_function_list`)
    [90m  7. [39m          └─function_list[[i]](value)
    [90m  8. [39m            ├─dplyr::filter(., Country == "Australia")
    [90m  9. [39m            └─tsibble:::filter.tbl_ts(., Country == "Australia")
    [90m 10. [39m              └─tsibble:::by_row(...)
    [90m 11. [39m                └─tsibble:::update_meta(tbl, .data, ordered = ordered, interval = interval(.data))
    [90m 12. [39m                  ├─tsibble:::restore_index_class(...)
    [90m 13. [39m                  │ └─tsibble::
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ══════════════════════════════════════════════════════
      [ OK: 30 | SKIPPED: 1 | WARNINGS: 31 | FAILED: 55 ]
      1. Failure: Automatic ARIMA selection (@test-arima.R#10) 
      2. Failure: Automatic ARIMA selection (@test-arima.R#19) 
      3. Failure: Automatic ARIMA selection (@test-arima.R#26) 
      4. Failure: Manual ARIMA selection (@test-arima.R#37) 
      5. Failure: Manual ARIMA selection (@test-arima.R#42) 
      6. Error: Manual ARIMA selection (@test-arima.R#47) 
      7. Failure: ARIMA with bad inputs (@test-arima.R#91) 
      8. Failure: ARIMA with bad inputs (@test-arima.R#97) 
      9. Failure: ARIMA with bad inputs (@test-arima.R#103) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking whether package ‘fable’ can be installed ... WARNING
    ```
    Found the following significant warnings:
      Warning: replacing previous import ‘fabletools::scale_x_yearmonth’ by ‘tsibble::scale_x_yearmonth’ when loading ‘fable’
      Warning: replacing previous import ‘fabletools::scale_x_yearweek’ by ‘tsibble::scale_x_yearweek’ when loading ‘fable’
      Warning: replacing previous import ‘fabletools::scale_x_yearquarter’ by ‘tsibble::scale_x_yearquarter’ when loading ‘fable’
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/fable/new/fable.Rcheck/00install.out’ for details.
    ```

# fabletools

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2019-09-16 10:50:02 UTC
* Number of recursive dependencies: 82

Run `revdep_details(,"fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
        intersect, setdiff, setequal, union
    
    Warning: Incompatible methods ("<.Date", "<.vctrs_vctr") for "<"
    Error: Detecting a corrupt tsibble object, and please reconstruct with `as_tsibble()`.
    Backtrace:
    [90m     [39m█
    [90m  1. [39m└─`%>%`(...)
    [90m  2. [39m  ├─base::withVisible(eval(quote(`_fseq`(`_lhs`)), env, env))
    [90m  3. [39m  └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
    [90m  4. [39m    └─base::eval(quote(`_fseq`(`_lhs`)), env, env)
    [90m  5. [39m      └─`_fseq`(`_lhs`)
    [90m  6. [39m        └─magrittr::freduce(value, `_function_list`)
    [90m  7. [39m          └─function_list[[i]](value)
    [90m  8. [39m            ├─dplyr::filter(., Quarter < yearquarter("2006 Q1"))
    [90m  9. [39m            └─tsibble:::filter.tbl_ts(., Quarter < yearquarter("2006 Q1"))
    [90m 10. [39m              └─tsibble:::by_row(...)
    [90m 11. [39m                └─tsibble:::update_meta(tbl, .data, ordered = ordered, interval = interval(.data))
    [90m 12. [39m                  ├─tsibble:::restore_index_class(...)
    [90m 13. [39m                 
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       
      7: `units_since()` is deprecated as of tsibble 0.9.0.
      Please use `as.double()` instead.
      [90mThis warning is displayed once per session.[39m
      [90mCall `lifecycle::last_warnings()` to see where this warning was generated.[39m 
      8: 2 errors (1 unique) encountered for ets
      [2] No common type for `..1` <interval> and `..2` <double>.
       
      9: 2 errors (1 unique) encountered for lm
      [2] No common type for `..1` <interval> and `..2` <double>.
       
      10: 1 error encountered for var
      [1] No common type for `..1` <interval> and `..2` <double>.
       
      Execution halted
    ```

*   checking for unstated dependencies in examples ... WARNING
    ```
    Registered S3 methods overwritten by 'fabletools':
      method                 from   
      scale_type.yearmonth   tsibble
      scale_type.yearquarter tsibble
      scale_type.yearweek    tsibble
    ```

# feasts

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/feasts
* URL: http://feasts.tidyverts.org/
* BugReports: https://github.com/tidyverts/feasts/issues
* Date/Publication: 2019-09-02 14:10:06 UTC
* Number of recursive dependencies: 80

Run `revdep_details(,"feasts")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    
        scale_x_yearmonth, scale_x_yearquarter, scale_x_yearweek
    
    > library(tsibbledata)
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > 
    > vic_elec %>% ACF(Temperature)
    Error in !is_regular(.data) : invalid argument type
    Calls: %>% ... <Anonymous> -> ACF -> build_cf -> check_gaps -> has_gaps
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ══════════════════════════════════════════════════════
      [ OK: 9 | SKIPPED: 2 | WARNINGS: 8 | FAILED: 20 ]
      1. Error: ACF (@test-cf.R#13) 
      2. Error: PACF (@test-cf.R#43) 
      3. Error: CCF (@test-cf.R#74) 
      4. Error: Additive classical decomposition (@test-classical.R#5) 
      5. Error: Multiplicative classical decomposition (@test-classical.R#28) 
      6. Error: guerrero() (@test-features.R#8) 
      7. Error: unit root features (@test-features.R#18) 
      8. Error: basic features (@test-features.R#29) 
      9. Error: *cf features (@test-features.R#40) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking R code for possible problems ... NOTE
    ```
    gg_season: no visible binding for global variable ‘id’
    gg_subseries: no visible binding for global variable ‘id’
    Undefined global functions or variables:
      id
    ```

# fpp3

<details>

* Version: 0.1
* Source code: https://github.com/cran/fpp3
* URL: https://github.com/robjhyndman/fpp3-package, https://OTexts.org/fpp3/
* BugReports: https://github.com/robjhyndman/fpp3-package
* Date/Publication: 2019-10-09 15:00:05 UTC
* Number of recursive dependencies: 53

Run `revdep_details(,"fpp3")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > 
    > aus_airpassengers
    Error: Detecting a corrupt tsibble object, and please reconstruct with `as_tsibble()`.
    Backtrace:
    [90m     [39m█
    [90m  1. [39m├─(function (x, ...) ...
    [90m  2. [39m└─tibble:::print.tbl(x)
    [90m  3. [39m  ├─tibble:::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
    [90m  4. [39m  │ ├─base::cat(paste0(..., "\n"), sep = "")
    [90m  5. [39m  │ └─base::paste0(..., "\n")
    [90m  6. [39m  ├─base::format(x, ..., n = n, width = width, n_extra = n_extra)
    [90m  7. [39m  └─tsibble:::format.tbl_ts(x, ..., n = n, width = width, n_extra = n_extra)
    [90m  8. [39m    ├─base::format(trunc_mat(x, n = n, width = width, n_extra = n_extra))
    [90m  9. [39m    └─tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra)
    [90m 10. [39m      ├─base::as.data.frame(head(x, n))
    [90m 11. [39m      ├─utils::head(x, n)
    [90m 12. [39m      └─utils:::head.data.frame(x, n)
    [90m 13. [39m        ├─x[seq_len(n), , drop = FALSE]
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘fable’ ‘fabletools’ ‘feasts’ ‘ggplot2’ ‘lubridate’ ‘tibble’
      ‘tidyr’ ‘tsibble’ ‘tsibbledata’
      All declared Imports should be used.
    ```

# pmdplyr

<details>

* Version: 0.3.0
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2019-08-26 09:50:02 UTC
* Number of recursive dependencies: 96

Run `revdep_details(,"pmdplyr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      12: stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
      13: stop_incompatible(x, y, details = details, ..., x_arg = x_arg, y_arg = to_arg, 
             message = message, .subclass = c(.subclass, "vctrs_error_incompatible_cast"))
      14: stop_vctrs(message, .subclass = c(.subclass, "vctrs_error_incompatible"), x = x, 
             y = y, details = details, ...)
      15: abort(message, .subclass = c(.subclass, "vctrs_error"), ...)
      16: signal_abort(cnd)
      
      ══ testthat results  ══════════════════════════════════════════════════════
      [ OK: 297 | SKIPPED: 0 | WARNINGS: 16 | FAILED: 2 ]
      1. Error: panel_convert input failstates (@test-bad_input.R#166) 
      2. Error: pibble tsibble conversion (@test-panel_convert.R#23) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# tsibbledata

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/tsibbledata
* URL: http://tsibbledata.tidyverts.org/
* BugReports: https://github.com/tidyverts/tsibbledata/issues
* Date/Publication: 2019-06-15 07:30:03 UTC
* Number of recursive dependencies: 46

Run `revdep_details(,"tsibbledata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### ** Examples
    > 
    > 
    > vic_elec
    Error: Detecting a corrupt tsibble object, and please reconstruct with `as_tsibble()`.
    Backtrace:
    [90m     [39m█
    [90m  1. [39m├─(function (x, ...) ...
    [90m  2. [39m└─tibble:::print.tbl(x)
    [90m  3. [39m  ├─tibble:::cat_line(format(x, ..., n = n, width = width, n_extra = n_extra))
    [90m  4. [39m  │ ├─base::cat(paste0(..., "\n"), sep = "")
    [90m  5. [39m  │ └─base::paste0(..., "\n")
    [90m  6. [39m  ├─base::format(x, ..., n = n, width = width, n_extra = n_extra)
    [90m  7. [39m  └─tsibble:::format.tbl_ts(x, ..., n = n, width = width, n_extra = n_extra)
    [90m  8. [39m    ├─base::format(trunc_mat(x, n = n, width = width, n_extra = n_extra))
    [90m  9. [39m    └─tibble::trunc_mat(x, n = n, width = width, n_extra = n_extra)
    [90m 10. [39m      ├─base::as.data.frame(head(x, n))
    [90m 11. [39m      ├─utils::head(x, n)
    [90m 12. [39m      └─utils:::head.data.frame(x, n)
    [90m 13. [39m        ├─x[seq_len(n), , drop = FALSE]
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tsibble’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2 marked UTF-8 strings
    ```

