# nullabor

Version: 0.3.5

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forecast’ ‘rlang’ ‘tidyverse’ ‘tsibble’
      All declared Imports should be used.
    ```

# sugrrants

Version: 0.2.3

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > 
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > # compute the calendar layout for the data frame
    > calendar_df <- pedestrian %>%
    +   filter(Sensor_ID == 13, Year == 2016) %>%
    +   frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 4)
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘dplyr’ 0.8.0.1 is already loaded, but >= 0.8.0.9008 is required
    Calls: %>% ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ══════════════════════════════════
      OK: 8 SKIPPED: 0 FAILED: 15
      1. Error: Multiple y's and NA's (@test-calendar.R#11) 
      2. Error: Variable scoping (@test-calendar.R#30) 
      3. Failure: Some column names of data are used in the function (@test-calendar.R#38) 
      4. Failure: Some column names of data are used in the function (@test-calendar.R#43) 
      5. Failure: Some column names of data are used in the function (@test-calendar.R#48) 
      6. Failure: Some column names of data are used in the function (@test-calendar.R#53) 
      7. Error: Some column names of data are used in the function (@test-calendar.R#58) 
      8. Error: The argument calendar (@test-calendar.R#83) 
      9. Error: The tsibble data (@test-calendar.R#124) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error in re-building vignettes:
      ...
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union
    
    Loading required package: viridisLite
    Loading required package: ggplot2
    Quitting from lines 42-47 (frame-calendar.Rmd) 
    Error: processing vignette 'frame-calendar.Rmd' failed with diagnostics:
    namespace 'dplyr' 0.8.0.1 is already loaded, but >= 0.8.0.9008 is required
    Execution halted
    ```

