## Test environments

* local OS X install, R 3.5.0
* ubuntu 16.04 (on travis-ci), R oldrel, release, devel
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## Reverse dependencies

I have run R CMD check on the 2 downstream dependencies. 

* The `sugrrants` package had 1 error and 1 warning due to incompatibility with ggplot2 v3.0.0, which is unrelated to this release.
* The `tsbox` package that I could install passed the checks.
