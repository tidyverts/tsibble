This submission fixes issues when system time zone is set to "Europe/London".

## Test environments

* local OS X install, R 3.5.1
* ubuntu 16.04 (on travis-ci), R oldrel, release, devel
* win-builder (devel)
* fedora (on rhub)

## R CMD check results

0 errors | 0 warnings | 1 note

## Reverse dependencies

I have run R CMD check on the 3 downstream dependencies. 

* All packages that I could install passed the checks.
