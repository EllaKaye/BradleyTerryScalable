## Test environments
* local OS X install, R 3.3.2
* Ubuntu 12.04.5 (on travis-ci), R 3.4.0
* win-builder (release and devel)

## R CMD check results
here were no ERRORs and no WARNINGs

There was one NOTE on the linux check (on Travis-CI):

* checking installed package size ... NOTE
  installed size is  5.7Mb
  sub-directories of 1Mb or more:
    libs   5.4Mb

This NOTE does not appear on the OS X or windows check. 
Moreover, this NOTE also did not appear when checking on a local linux environment (using platform: i686-pc-linux-gnu (32-bit))

My understanding is that this inflation of the libs subdirectory is due to the use of Rcpp. Indeed, some functions of the BradleyTerryScalable package have been written in C++ using Rcpp. They are needed to fit the Bradley-Terry model to large datasets. Without the speed up gained from those C++ functions, this package would not be fit for purpose.


There was one NOTE from the windows check (but not on the OS X or linux check): 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ella Kaye <E.Kaye.1@warwick.ac.uk>'

My understanding is that is because this is the first submission of this package on CRAN

## First submission
* This is the first submission of this package 
* There are therefore no reverse dependencies to check
