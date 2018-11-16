## Test environments

* local OS X install, R 3.5.1
* ubuntu 16.04 (on travis-ci), R oldrel, release, devel
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

I have run R CMD check on the 3 downstream dependencies. 

* The `nullabor` & `tsbox` packages that I could install passed the checks.
* Two unit tests failed in the **sugrrants** package. As the maintainer of **sugrrants**, it will be on CRAN, following **tsibble**.
