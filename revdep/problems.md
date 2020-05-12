# ezplot

<details>

* Version: 0.5.0
* Source code: https://github.com/cran/ezplot
* Date/Publication: 2020-05-09 19:50:02 UTC
* Number of recursive dependencies: 99

Run `revdep_details(,"ezplot")` for more info

</details>

## Newly broken

*   checking whether package ‘ezplot’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/ezplot/new/ezplot.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘tsibble’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘ezplot’ ...
** package ‘ezplot’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.2.99.9011 is required
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> namespaceImport -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘ezplot’
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/ezplot/new/ezplot.Rcheck/ezplot’

```
### CRAN

```
* installing *source* package ‘ezplot’ ...
** package ‘ezplot’ successfully unpacked and MD5 sums checked
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
* DONE (ezplot)

```
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
Error: package or namespace load failed for ‘fabletools’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.3.0 is required
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
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.3.0 is required
Calls: <Anonymous> ... namespaceImport -> loadNamespace -> namespaceImport -> loadNamespace
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
Error: package or namespace load failed for ‘fabletools’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.3.0 is required
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
  error: package or namespace load failed for ‘dplyr’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 namespace ‘vctrs’ 0.2.4 is already loaded, but >= 0.2.99.9011 is required
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
    Running examples in ‘gravitas-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: create_gran
    > ### Title: Build dynamic temporal granularities
    > ### Aliases: create_gran
    > 
    > ### ** Examples
    > 
    > library(dplyr)
    Error: package or namespace load failed for ‘dplyr’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.2.99.9011 is required
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 45 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 34 ]
      1. Error: build_gran expected output minute_hhour (@test-build_gran.R#27) 
      2. Error: build_gran expected output month_semester (@test-build_gran.R#35) 
      3. Error: build_gran expected output week_semester (@test-build_gran.R#43) 
      4. Error: build_gran expected output second_hhour (@test-build_gran.R#47) 
      5. Error: (unknown) (@test-create_gran.R#8) 
      6. Error: day_fortnight outputs a numeric value (@test-day-order-up.R#13) 
      7. Error: day_fortnight expected output (@test-day-order-up.R#17) 
      8. Error: day_fortnight output length equals input length (@test-day-order-up.R#21) 
      9. Error: day_semester outputs a numeric value (@test-day-order-up.R#32) 
      1. ...
      
      Error: testthat unit tests failed
      Execution halted
    ```

# nullabor

<details>

* Version: 0.3.9
* Source code: https://github.com/cran/nullabor
* URL: http://github.com/dicook/nullabor
* BugReports: http://github.com/dicook/nullabor/issues
* Date/Publication: 2020-02-25 21:50:02 UTC
* Number of recursive dependencies: 94

Run `revdep_details(,"nullabor")` for more info

</details>

## Newly broken

*   checking whether package ‘nullabor’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/nullabor/new/nullabor.Rcheck/00install.out’ for details.
    ```

## Newly fixed

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘forecast’ ‘rlang’ ‘tsibble’ ‘viridis’
      All declared Imports should be used.
    ```

## Installation

### Devel

```
* installing *source* package ‘nullabor’ ...
** package ‘nullabor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.2.99.9011 is required
Calls: <Anonymous> ... asNamespace -> loadNamespace -> namespaceImport -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘nullabor’
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/nullabor/new/nullabor.Rcheck/nullabor’

```
### CRAN

```
* installing *source* package ‘nullabor’ ...
** package ‘nullabor’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** demo
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
* DONE (nullabor)

```
# pmdplyr

<details>

* Version: 0.3.1
* Source code: https://github.com/cran/pmdplyr
* URL: https://nickch-k.github.io/pmdplyr, https://github.com/NickCH-K/pmdplyr
* BugReports: https://github.com/NickCH-K/pmdplyr/issues
* Date/Publication: 2020-03-09 19:30:02 UTC
* Number of recursive dependencies: 101

Run `revdep_details(,"pmdplyr")` for more info

</details>

## Newly broken

*   checking whether package ‘pmdplyr’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/pmdplyr/new/pmdplyr.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘pmdplyr’ ...
** package ‘pmdplyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error: package or namespace load failed for ‘dplyr’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
 namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.2.99.9011 is required
Error: package ‘dplyr’ could not be loaded
Execution halted
ERROR: lazy loading failed for package ‘pmdplyr’
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/pmdplyr/new/pmdplyr.Rcheck/pmdplyr’

```
### CRAN

```
* installing *source* package ‘pmdplyr’ ...
** package ‘pmdplyr’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (pmdplyr)

```
# RTL

<details>

* Version: 0.1.1
* Source code: https://github.com/cran/RTL
* URL: https://github.com/risktoollib/RTL
* Date/Publication: 2020-02-23 18:50:02 UTC
* Number of recursive dependencies: 138

Run `revdep_details(,"RTL")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘RTL-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: bond
    > ### Title: 'bond'
    > ### Aliases: bond
    > 
    > ### ** Examples
    > 
    > bond(ytm = 0.05, C = 0.05,T2M = 1,m = 2,output = "price")
    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.2.99.9011 is required
    Calls: bond ... getNamespace -> loadNamespace -> namespaceImport -> loadNamespace
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

*   checking whether package ‘sugrrants’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/sugrrants/new/sugrrants.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘sugrrants’ ...
** package ‘sugrrants’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
** inst
** byte-compile and prepare package for lazy loading
Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
  namespace ‘vctrs’ 0.2.4 is already loaded, but >= 0.2.99.9011 is required
Calls: <Anonymous> ... asNamespace -> loadNamespace -> namespaceImport -> loadNamespace
Execution halted
ERROR: lazy loading failed for package ‘sugrrants’
* removing ‘/Users/earo/Rpkg/tsibble/revdep/checks.noindex/sugrrants/new/sugrrants.Rcheck/sugrrants’

```
### CRAN

```
* installing *source* package ‘sugrrants’ ...
** package ‘sugrrants’ successfully unpacked and MD5 sums checked
** using staged installation
** R
** data
*** moving datasets to lazyload DB
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
* DONE (sugrrants)

```
# tsbox

<details>

* Version: 0.2.1
* Source code: https://github.com/cran/tsbox
* URL: https://www.tsbox.help
* BugReports: https://github.com/christophsax/tsbox/issues
* Date/Publication: 2020-04-29 19:20:03 UTC
* Number of recursive dependencies: 93

Run `revdep_details(,"tsbox")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > head(x.ts)
         mdeaths fdeaths
    [1,]    2134     901
    [2,]    1863     689
    [3,]    1877     827
    [4,]    1877     677
    [5,]    1492     522
    [6,]    1249     406
    > head(ts_df(x.ts))
           id       time value
    1 mdeaths 1974-01-01  2134
    2 mdeaths 1974-02-01  1863
    3 mdeaths 1974-03-01  1877
    4 mdeaths 1974-04-01  1877
    5 mdeaths 1974-05-01  1492
    6 mdeaths 1974-06-01  1249
    > 
    > suppressMessages(library(dplyr))
    Error: package or namespace load failed for ‘dplyr’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.2.99.9011 is required
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 355 | SKIPPED: 16 | WARNINGS: 0 | FAILED: 13 ]
      1. Error: arithmetic operations work properly (@test_arithmetic.R#31) 
      2. Error: df aggregation using date_ functions is working (@test_date_utils.R#10) 
      3. Error: time_shift is working (@test_date_utils.R#47) 
      4. Error: works with df with improper col classes (@test_dirty.R#8) 
      5. Error: time column of daily data is treated as Date (#114) (@test_dirty.R#25) 
      6. Error: minimal example works (@test_nyc_flights.R#10) 
      7. Error: colname guessing works as expected (@test_object_conversion.R#187) 
      8. Error: conversions work with multiple ids 
      9. Error: (unknown) (@test_time_conversion.R#4) 
      1. ...
      
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
    Error: package or namespace load failed for ‘tsibble’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     namespace ‘vctrs’ 0.2.4 is already loaded, but >= 0.3.0 is required
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

# TSstudio

<details>

* Version: 0.1.6
* Source code: https://github.com/cran/TSstudio
* URL: https://github.com/RamiKrispin/TSstudio
* BugReports: https://github.com/RamiKrispin/TSstudio/issues
* Date/Publication: 2020-01-21 05:30:02 UTC
* Number of recursive dependencies: 136

Run `revdep_details(,"TSstudio")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘TSstudio-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Coffee_Prices
    > ### Title: Coffee Prices: Robusta and Arabica
    > ### Aliases: Coffee_Prices
    > ### Keywords: datasets
    > 
    > ### ** Examples
    > 
    > ts_plot(Coffee_Prices)
    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      namespace ‘vctrs’ 0.2.4 is already loaded, but >= 0.2.99.9011 is required
    Calls: ts_plot ... asNamespace -> loadNamespace -> namespaceImport -> loadNamespace
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘forecastHybrid’
      All declared Imports should be used.
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
    Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
      namespace ‘vctrs’ 0.2.4 is being loaded, but >= 0.2.99.9011 is required
    Calls: extract_grid ... getNamespace -> loadNamespace -> namespaceImport -> loadNamespace
    Execution halted
    ```

*   checking tests ...
    ```
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      [90m 10. [39mbase::getExportedValue(pkg, name)
      [90m 11. [39mbase::asNamespace(ns)
      [90m 12. [39mbase::getNamespace(ns)
      [90m 13. [39mbase::loadNamespace(name)
      [90m 15. [39mbase::loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
      
      ══ testthat results  ═══════════════════════════════════════════════════════════
      [ OK: 0 | SKIPPED: 0 | WARNINGS: 0 | FAILED: 4 ]
      1. Error: Test basic object (@test-obj_class.R#5) 
      2. Error: Using arguments - tsibble with hourly aggregation and two variables (@test-obj_class.R#14) 
      3. Error: Using arguments - xts with monthly aggregation, pulling two variables (@test-obj_class.R#23) 
      4. Error: Using arguments - tsibble with monthly aggregation, pulling two variables (@test-obj_class.R#33) 
      
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

*   checking examples ... ERROR
    ```
    Running examples in ‘USgrid-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: Cal_elec
    > ### Title: Demand for California Independent System Operator (CISO)
    > ### Aliases: Cal_elec
    > ### Keywords: datasets time-series
    > 
    > ### ** Examples
    > 
    > 
    > data(Cal_elec)
    > 
    > library(plotly)
    Loading required package: ggplot2
    Error: package or namespace load failed for ‘plotly’ in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]):
     namespace ‘vctrs’ 0.2.4 is already loaded, but >= 0.2.99.9011 is required
    Execution halted
    ```

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
      [90m 10. [39mbase::loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]])
      
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

