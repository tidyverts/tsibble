## Test environments

* local mac OS install, R 4.0.3
* ubuntu 20.04 (on github actions), R-devel, R 4.0.2, R 3.6.3, 3.5.3, R 3.4.4, R 3.3.3
* mac OS 10.15.4 (on github actions) R-devel, R 4.0.2
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 4.0.3

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## revdepcheck results

We checked 22 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems

* 5 unit tests in the {brolgar} package have been broken due to the fact that the author didn't set the seed for random generation.
