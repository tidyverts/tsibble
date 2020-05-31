## Release summary

This release will be compatible with dplyr v1.0.0. Some failing packages are related to dplyr updates.

The underlying tsibble data structure has also changed. Some failing packages that provide tsibble data examples are required to reconstruct their tsibble objects.

## Test environments

* local mac OS install, R 4.0.0
* ubuntu 16.04 (on github actions), R-devel, R 4.0.0, R 3.6.3, 3.5.3, R 3.4.4, R 3.3.3
* mac OS 10.15.4 (on github actions) R-devel, R 3.6.0
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.0.0
* win-builder (devel)

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## revdepcheck results

We checked 17 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 7 new problems
 * We failed to check 4 packages
