# fabletools

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2019-08-08 14:30:02 UTC
* Number of recursive dependencies: 81

Run `revdep_details(,"fabletools")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘fabletools-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: aggregate_key
    > ### Title: Expand a dataset to include other levels of aggregation
    > ### Aliases: aggregate_key
    > 
    > ### ** Examples
    > 
    > library(tsibble)
    > tourism %>% 
    +   aggregate_key(Purpose * (State / Region), Trips = sum(Trips))
    Error in build_tsibble_meta(.data, key_data = key_dt, index = as_string(idx),  : 
      inherits(interval, "interval") is not TRUE
    Calls: %>% ... %>% -> eval -> eval -> build_tsibble_meta -> stopifnot
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
             index2 = as_string(idx), ordered = TRUE) %>% mutate(!!!set_names(map(kv, 
             function(x) expr(agg_key(!!sym(x)))), kv))
      12: eval(lhs, parent, parent)
      13: eval(lhs, parent, parent)
      14: build_tsibble_meta(.data, key_data = key_dt, index = as_string(idx), 
             index2 = as_string(idx), ordered = TRUE)
      15: stopifnot(inherits(interval, "interval"))
      
      ══ testthat results  ═════════════════════════════
      [ OK: 285 | SKIPPED: 1 | WARNINGS: 0 | FAILED: 2 ]
      1. Error: Model response identification (@test-parser.R#110) 
      2. Error: reconciliation (@test-reconciliation.R#4) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

