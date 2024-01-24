# fable

<details>

* Version: 0.3.3
* GitHub: https://github.com/tidyverts/fable
* Source code: https://github.com/cran/fable
* Date/Publication: 2023-03-22 16:00:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::revdep_details(, "fable")` for more info

</details>

## In both

*   checking whether package ‘fable’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Documents/rpkgs/tsibble/revdep/checks.noindex/fable/new/fable.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘fable’ ...
** package ‘fable’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using SDK: ‘MacOSX14.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/tsibble/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c etsTargetFunction.cpp -o etsTargetFunction.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/tsibble/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c etsTargetFunctionWrapper.cpp -o etsTargetFunctionWrapper.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/tsibble/new/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c etscalc.c -o etscalc.o
...
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o fable.so etsTargetFunction.o etsTargetFunctionWrapper.o etscalc.o registerDynamicSymbol.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
ld: warning: -multiply_defined is obsolete
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [fable.so] Error 1
ERROR: compilation failed for package ‘fable’
* removing ‘/Users/earo/Documents/rpkgs/tsibble/revdep/checks.noindex/fable/new/fable.Rcheck/fable’


```
### CRAN

```
* installing *source* package ‘fable’ ...
** package ‘fable’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using SDK: ‘MacOSX14.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c etsTargetFunction.cpp -o etsTargetFunction.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c etsTargetFunctionWrapper.cpp -o etsTargetFunctionWrapper.o
clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/fable/Rcpp/include' -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c etscalc.c -o etscalc.o
...
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o fable.so etsTargetFunction.o etsTargetFunctionWrapper.o etscalc.o registerDynamicSymbol.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
ld: warning: -multiply_defined is obsolete
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [fable.so] Error 1
ERROR: compilation failed for package ‘fable’
* removing ‘/Users/earo/Documents/rpkgs/tsibble/revdep/checks.noindex/fable/old/fable.Rcheck/fable’


```
# UComp

<details>

* Version: 4.0.2
* GitHub: NA
* Source code: https://github.com/cran/UComp
* Date/Publication: 2023-05-20 07:30:09 UTC
* Number of recursive dependencies: 83

Run `revdepcheck::revdep_details(, "UComp")` for more info

</details>

## In both

*   checking whether package ‘UComp’ can be installed ... ERROR
    ```
    Installation failed.
    See ‘/Users/earo/Documents/rpkgs/tsibble/revdep/checks.noindex/UComp/new/UComp.Rcheck/00install.out’ for details.
    ```

## Installation

### Devel

```
* installing *source* package ‘UComp’ ...
** package ‘UComp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using SDK: ‘MacOSX14.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/tsibble/new/Rcpp/include' -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/UComp/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/tsibble/new/Rcpp/include' -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/UComp/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c UCompC.cpp -o UCompC.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o UComp.so RcppExports.o UCompC.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
ld: warning: -multiply_defined is obsolete
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [UComp.so] Error 1
ERROR: compilation failed for package ‘UComp’
* removing ‘/Users/earo/Documents/rpkgs/tsibble/revdep/checks.noindex/UComp/new/UComp.Rcheck/UComp’


```
### CRAN

```
* installing *source* package ‘UComp’ ...
** package ‘UComp’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
using C++ compiler: ‘Apple clang version 15.0.0 (clang-1500.1.0.2.5)’
using SDK: ‘MacOSX14.2.sdk’
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/UComp/Rcpp/include' -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/UComp/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -arch arm64 -std=gnu++17 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG  -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/UComp/Rcpp/include' -I'/Users/earo/Documents/rpkgs/tsibble/revdep/library.noindex/UComp/RcppArmadillo/include' -I/opt/R/arm64/include     -fPIC  -falign-functions=64 -Wall -g -O2  -c UCompC.cpp -o UCompC.o
clang++ -arch arm64 -std=gnu++17 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o UComp.so RcppExports.o UCompC.o -L/Library/Frameworks/R.framework/Resources/lib -lRlapack -L/Library/Frameworks/R.framework/Resources/lib -lRblas -L/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0 -L/opt/gfortran/lib -lgfortran -lemutls_w -lquadmath -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
ld: warning: -single_module is obsolete
ld: warning: -multiply_defined is obsolete
ld: warning: search path '/opt/gfortran/lib/gcc/aarch64-apple-darwin20.0/12.2.0' not found
ld: warning: search path '/opt/gfortran/lib' not found
ld: library 'gfortran' not found
clang: error: linker command failed with exit code 1 (use -v to see invocation)
make: *** [UComp.so] Error 1
ERROR: compilation failed for package ‘UComp’
* removing ‘/Users/earo/Documents/rpkgs/tsibble/revdep/checks.noindex/UComp/old/UComp.Rcheck/UComp’


```
