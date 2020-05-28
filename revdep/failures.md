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
