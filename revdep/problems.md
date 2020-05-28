# fable

<details>

* Version: 0.2.0
* Source code: https://github.com/cran/fable
* URL: https://fable.tidyverts.org, https://github.com/tidyverts/fable
* BugReports: https://github.com/tidyverts/fable/issues
* Date/Publication: 2020-04-22 13:12:08 UTC
* Number of recursive dependencies: 99

Run `revdep_details(,"fable")` for more info

</details>

## Newly broken

*   checking whether package ‘fable’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/fable/new/fable.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fable’ ...
** package ‘fable’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c etsTargetFunction.cpp -o etsTargetFunction.o
In file included from etsTargetFunction.cpp:3:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp.h:27:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/RcppCommon.h:128:
/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp/exceptions.h:112:53: warning: all paths through this function will call itself [-Winfinite-recursion]
    inline void warning(const std::string& message) {        // #nocov start
                                                    ^
1 warning generated.
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c etsTargetFunctionWrapper.cpp -o etsTargetFunctionWrapper.o
In file included from etsTargetFunctionWrapper.cpp:10:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp.h:27:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/RcppCommon.h:128:
/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp/exceptions.h:112:53: warning: all paths through this function will call itself [-Winfinite-recursion]
    inline void warning(const std::string& message) {        // #nocov start
                                                    ^
1 warning generated.
clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c etscalc.c -o etscalc.o
clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o fable.so etsTargetFunction.o etsTargetFunctionWrapper.o etscalc.o registerDynamicSymbol.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0'
installing to /Users/earo/Rpkg/tsibble/revdep/checks.noindex/fable/new/fable.Rcheck/00LOCK-fable/00new/fable/libs
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘fabletools’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘dplyr’ 0.8.5 is being loaded, but >= 0.8.99 is required
Error: package ‘fabletools’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘fable’
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/fable/new/fable.Rcheck/fable’

```
### CRAN

```
* installing *source* package ‘fable’ ...
** package ‘fable’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c etsTargetFunction.cpp -o etsTargetFunction.o
In file included from etsTargetFunction.cpp:3:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp.h:27:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/RcppCommon.h:128:
/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp/exceptions.h:112:53: warning: all paths through this function will call itself [-Winfinite-recursion]
    inline void warning(const std::string& message) {        // #nocov start
                                                    ^
1 warning generated.
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c etsTargetFunctionWrapper.cpp -o etsTargetFunctionWrapper.o
In file included from etsTargetFunctionWrapper.cpp:10:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp.h:27:
In file included from /Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/RcppCommon.h:128:
/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include/Rcpp/exceptions.h:112:53: warning: all paths through this function will call itself [-Winfinite-recursion]
    inline void warning(const std::string& message) {        // #nocov start
                                                    ^
1 warning generated.
clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c etscalc.c -o etscalc.o
clang -mmacosx-version-min=10.13 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Rpkg/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/usr/local/include   -fPIC  -Wall -g -O2  -c registerDynamicSymbol.c -o registerDynamicSymbol.o
clang++ -mmacosx-version-min=10.13 -std=gnu++11 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/usr/local/lib -o fable.so etsTargetFunction.o etsTargetFunctionWrapper.o etscalc.o registerDynamicSymbol.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0 -L/usr/local/gfortran/lib -lgfortran -lquadmath -lm -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: directory not found for option '-L/usr/local/gfortran/lib/gcc/x86_64-apple-darwin18/8.2.0'
installing to /Users/earo/Rpkg/tsibble/revdep/checks.noindex/fable/old/fable.Rcheck/00LOCK-fable/00new/fable/libs
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** checking absolute paths in shared objects and dynamic libraries
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (fable)

```
# fabletools

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/fabletools
* URL: http://fabletools.tidyverts.org/, https://github.com/tidyverts/fabletools
* BugReports: https://github.com/tidyverts/fabletools/issues
* Date/Publication: 2020-03-24 07:10:02 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"fabletools")` for more info

</details>

## Newly broken

*   checking whether package ‘fabletools’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/fabletools/new/fabletools.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fabletools’ ...
** package ‘fabletools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
  namespace ‘dplyr’ 0.8.5 is being loaded, but >= 0.8.99 is required
Calls: <Anonymous> ... namespaceImportFrom -> asNamespace -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘fabletools’
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/fabletools/new/fabletools.Rcheck/fabletools’

```
### CRAN

```
* installing *source* package ‘fabletools’ ...
** package ‘fabletools’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (fabletools)

```
# feasts

<details>

* Version: 0.1.3
* Source code: https://github.com/cran/feasts
* URL: http://feasts.tidyverts.org/, https://github.com/tidyverts/feasts/
* BugReports: https://github.com/tidyverts/feasts/issues
* Date/Publication: 2020-03-18 07:00:11 UTC
* Number of recursive dependencies: 92

Run `revdep_details(,"feasts")` for more info

</details>

## Newly broken

*   checking whether package ‘feasts’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/feasts/new/feasts.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘feasts’ ...
** package ‘feasts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘fabletools’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘dplyr’ 0.8.5 is being loaded, but >= 0.8.99 is required
Error: package ‘fabletools’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘feasts’
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/feasts/new/feasts.Rcheck/feasts’

```
### CRAN

```
* installing *source* package ‘feasts’ ...
** package ‘feasts’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
*** copying figures
** building package indices
** installing vignettes
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (feasts)

```
# fpp3

<details>

* Version: 0.2
* Source code: https://github.com/cran/fpp3
* URL: https://github.com/robjhyndman/fpp3-package, https://OTexts.org/fpp3/
* BugReports: https://github.com/robjhyndman/fpp3-package
* Date/Publication: 2020-03-15 05:30:03 UTC
* Number of recursive dependencies: 63

Run `revdep_details(,"fpp3")` for more info

</details>

## Newly broken

*   checking whether package ‘fpp3’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/fpp3/new/fpp3.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘fable’ ‘fabletools’ ‘feasts’ ‘ggplot2’ ‘lubridate’ ‘tibble’
      ‘tidyr’ ‘tsibble’ ‘tsibbledata’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘fpp3’ ...
** package ‘fpp3’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
Error: package or namespace load failed for ‘fpp3’:
 .onAttach failed in attachNamespace() for 'fpp3', details:
  call: NULL
  error: package or namespace load failed for ‘tsibble’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
 namespace ‘dplyr’ 0.8.5 is already loaded, but >= 0.8.99 is required
Error: loading failed
Execution halted
ERROR: loading failed
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/fpp3/new/fpp3.Rcheck/fpp3’

```
### CRAN

```
* installing *source* package ‘fpp3’ ...
** package ‘fpp3’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** byte-compile and prepare package for lazy loading
** help
*** installing help indices
** building package indices
** testing if installed package can be loaded from temporary location
** testing if installed package can be loaded from final location
** testing if installed package keeps a record of temporary installation path
* DONE (fpp3)

```
# gravitas

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/gravitas
* URL: https://github.com/Sayani07/gravitas/
* BugReports: https://github.com/Sayani07/gravitas/issues
* Date/Publication: 2020-02-17 09:20:02 UTC
* Number of recursive dependencies: 94

Run `revdep_details(,"gravitas")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > library(dplyr)
    
    Attaching package: ‘dplyr’
    
    The following objects are masked from ‘package:stats’:
    
        filter, lag
    
    The following objects are masked from ‘package:base’:
    
        intersect, setdiff, setequal, union
    
    > library(ggplot2)
    > library(lvplot)
    > # Search for granularities
    > smart_meter10 %>%
    +   search_gran(highest_unit = "week")
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘dplyr’ 0.8.5 is already loaded, but >= 0.8.99 is required
    Calls: %>% ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 3. [39mbase:::tryCatchList(expr, classes, parentenv, handlers)
      [90m 4. [39mbase:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
      [90m 5. [39mvalue[[3L]](cond)
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 73 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 6 ]
      1. Error: (unknown) (@test-create_gran.R#9) 
      2. Error: (unknown) (@test-gran_advice.R#9) 
      3. Error: (unknown) (@test-harmony.R#9) 
      4. Error: (unknown) (@test-is_harmony.R#9) 
      5. Error: (unknown) (@test-prob_plot.R#6) 
      6. Error: (unknown) (@test-search_gran.R#8) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# pmdplyr

<details>

* Version: 0.3.1.1
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-05-15 08:47:17 UTC
* Number of recursive dependencies: 106

Run `revdep_details(,"pmdplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: panel_convert
    > 
    > ### ** Examples
    > 
    > # Only run examples if the relevant packages are installed
    > pkgs <- utils::installed.packages()
    > 
    > data(Scorecard)
    > 
    > # The example will turn a pibble to everything else
    > # But starting with another type will of course work!
    > S_pibble <- as_pibble(Scorecard, .i = unitid, .t = year)
    > 
    > # Get a tsibble
    > if ("tsibble" %in% pkgs) {
    +   head(panel_convert(S_pibble, to = "tsibble"))
    + }
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘dplyr’ 0.8.5 is already loaded, but >= 0.8.99 is required
    Calls: head ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [1mBacktrace:[22m
      [90m 1. [39mtsibble::tsibble
      [90m 2. [39mbase::getExportedValue(pkg, name)
      [90m 3. [39mbase::asNamespace(ns)
      [90m 4. [39mbase::getNamespace(ns)
      [90m 5. [39mbase::loadNamespace(name)
      [90m 8. [39mbase::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 290 | SKIPPED: 0 | WARNINGS: 13 | FAILED: 2 ]
      1. Error: panel_convert input failstates (@test-bad_input.R#165) 
      2. Error: (unknown) (@test-panel_convert.R#14) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# RTL

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RTL
* URL: https://github.com/risktoollib/RTL
* Date/Publication: 2020-02-23 18:50:02 UTC
* Number of recursive dependencies: 136

Run `revdep_details(,"RTL")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RTL-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: chart_zscore
    > ### Title: 'chart_zscore'
    > ### Aliases: chart_zscore
    > 
    > ### ** Examples
    > 
    > chart_zscore(df = ng_storage, title = "NG Storage Z Score",
    + per = "yearweek", output = "stl", chart = "seasons")
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘dplyr’ 0.8.5 is already loaded, but >= 0.8.99 is required
    Calls: chart_zscore ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘quantmod’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 15456 marked UTF-8 strings
    ```

# sugrrants

<details>

* Version: 0.2.7
* Source code: https://github.com/cran/sugrrants
* URL: https://pkg.earo.me/sugrrants
* BugReports: https://github.com/earowang/sugrrants/issues
* Date/Publication: 2020-04-18 05:20:03 UTC
* Number of recursive dependencies: 89

Run `revdep_details(,"sugrrants")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      namespace 'dplyr' 0.8.5 is already loaded, but >= 0.8.99 is required
      [1mBacktrace:[22m
      [90m  9. [39mtsibble::as_tsibble(., key = Sensor_Name, index = Date_Time)
      [90m 10. [39mbase::getExportedValue(pkg, name)
      [90m 11. [39mbase::asNamespace(ns)
      [90m 12. [39mbase::getNamespace(ns)
      [90m 13. [39mbase::loadNamespace(name)
      [90m 16. [39mbase::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 34 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 1 ]
      1. Error: The tsibble data (@test-calendar.R#124) 
      
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
* Number of recursive dependencies: 55

Run `revdep_details(,"tsibbledata")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘tsibbledata-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: olympic_running
    > ### Title: Fastest running times for Olympic races
    > ### Aliases: olympic_running
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    > library(tsibble)
    Error: package or namespace load failed for ‘tsibble’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
     namespace ‘dplyr’ 0.8.5 is already loaded, but >= 0.8.99 is required
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

# UKgrid

<details>

* Version: 0.1.2
* Source code: https://github.com/cran/UKgrid
* URL: https://github.com/RamiKrispin/UKgrid
* BugReports: https://github.com/RamiKrispin/UKgrid/issues
* Date/Publication: 2019-12-10 15:50:06 UTC
* Number of recursive dependencies: 136

Run `revdep_details(,"UKgrid")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘UKgrid-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: extract_grid
    > ### Title: Extracting and Aggregation of the UKgrid Dataset
    > ### Aliases: extract_grid
    > 
    > ### ** Examples
    > 
    > df <- extract_grid(type = "tsibble", columns = "ND", start = 2017)
    Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) : 
      namespace ‘dplyr’ 0.8.5 is already loaded, but >= 0.8.99 is required
    Calls: extract_grid ... namespaceImportFrom -> asNamespace -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 2. [39mtsibble::yearmonth
      [90m 3. [39mbase::getExportedValue(pkg, name)
      [90m 4. [39mbase::asNamespace(ns)
      [90m 5. [39mbase::getNamespace(ns)
      [90m 6. [39mbase::loadNamespace(name)
      [90m 9. [39mbase::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 3 | SKIPPED: 0 | WARNINGS: 1 | FAILED: 3 ]
      1. Error: Test basic object (@test-obj_class.R#5) 
      2. Error: Using arguments - tsibble with hourly aggregation and two variables (@test-obj_class.R#14) 
      3. Error: Using arguments - tsibble with monthly aggregation, pulling two variables (@test-obj_class.R#33) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

# USgrid

<details>

* Version: 0.1.0
* Source code: https://github.com/cran/USgrid
* URL: https://github.com/RamiKrispin/USgrid
* BugReports: https://github.com/RamiKrispin/USgrid/issues
* Date/Publication: 2020-01-24 17:20:06 UTC
* Number of recursive dependencies: 104

Run `revdep_details(,"USgrid")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m  4. [39mtsibble::is_tsibble
      [90m  5. [39mbase::getExportedValue(pkg, name)
      [90m  6. [39mbase::asNamespace(ns)
      [90m  7. [39mbase::getNamespace(ns)
      [90m  8. [39mbase::loadNamespace(name)
      [90m 11. [39mbase::loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]])
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 6 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 3 ]
      1. Error: Test US_elec dataset (@test-dataset.R#9) 
      2. Error: Test US_source dataset (@test-dataset.R#22) 
      3. Error: Test Cal_elec dataset (@test-dataset.R#35) 
      
      Error: testthat unit tests failed
      Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tsibble’
      All declared Imports should be used.
    ```

