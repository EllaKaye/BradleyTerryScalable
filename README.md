
<!-- README.md is generated from README.Rmd. Please edit that file -->
BradleyTerryScalable
====================

[![Travis-CI Build Status](https://travis-ci.org/EllaKaye/BradleyTerryScalable.svg?branch=master)](https://travis-ci.org/EllaKaye/BradleyTerryScalable) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/BradleyTerryScalable)](https://cran.r-project.org/package=BradleyTerryScalable)

An R package for fitting the Bradley-Terry model to pair-comparison data, to enable statistically principled ranking of a potentially large number of objects.

Given a number of items for whom we have pair-comparison data, the Bradley-Terry model assigns a 'strength' parameter to each item. These can be used to rank the items. Moreover, they can be used to determine the probability that any given item will 'beat' any other given item when they are compared. Further details of the mathematical model, and the algorithms used to fit them, will soon be available in a vignette (see the end of this `README` for vignette details).

**Note: this package is currently in the final stages of development. Some changes are likely in the user interface and documentation, especially, prior to the package's scheduled initial submission to CRAN in late June 2017.**

**Documentation website can be found [here](https://ellakaye.github.io/BradleyTerryScalable/) (thanks to [`pkgdown`](https://github.com/hadley/pkgdown))**

Installing the package
======================

``` r
library(devtools)
devtools::install_github("EllaKaye/BradleyTerryScalable", build_vignettes = TRUE)
```

``` r
library(BradleyTerryScalable)
```

Preparing the data
==================

The main fitting function in this package is `btfit`. This function takes as its main argument an object of class `btdata`. To create a `btdata` object, use the function `btdata(x)`.

The `x` argument to `btdata` can be one of three classes of object:

-   A data frame (`data.frame` or `tibble`), with three or four columns
    -   If the data frame has three columns, then the first column must be the name of item 1, the second column must be the name of item 2, and the third column must be the number of times item 1 has beaten item 2.
    -   If the data frame has four columns, then the first column must be the name of item 1, the second column must be the name of item 2, and the third column must be the number of times item 1 has beaten item 2 and the fourth column must be the number of times item 2 has beaten item 1.
-   A matrix (either a base `matrix` or a class from the `Matrix` package), dimension K by K, where K is the number of items. The *i,j*-th element is the number of times item *i* has beaten item *j*.

-   An igraph, representing the *comparison graph*, with the K items as nodes. For the edges:
    -   If the graph is unweighted, a directed edge from node *i* to node *j* for every time item *i* has beaten item *j*
    -   If the graph is weighted, then one edge node *i* to node *j* if item *i* has beaten item *j* at least once, with the weight attribute of that edge set to the number of times *i* has beaten *j*.

The `BradleyTerryScalable` package provides two toy data sets with which we'll use in this demonstration:

``` r
data(citations)
citations
#>               citing
#> cited          Biometrika Comm Statist JASA JRSS-B
#>   Biometrika          714          730  498    221
#>   Comm Statist         33          425   68     17
#>   JASA                320          813 1072    142
#>   JRSS-B              284          276  325    188
data(toy_data)
toy_data
#>    player1 player2 wins
#> 1        c       a    1
#> 2        a       b    2
#> 3        e       b    3
#> 4        d       c    3
#> 5        b       d    1
#> 6        e       d    4
#> 7        e       f    1
#> 8        g       f    5
#> 9        h       f    5
#> 10       e       g    3
#> 11       f       g    3
#> 12       h       g    2
#> 13       g       h    1
```

Run `btdata()` to produce objects of class `btdata`:

``` r
citations_btdata <- btdata(citations)
toy_btdata <- btdata(toy_data)
```

`btdata` objects are a list containing two elements:

-   `wins` which is a matrix of the form described in the second bullet point above
-   `components` which is itself a list of the fully-connected components of the comparison graph (see the third bullet point above)

Information about the `btdata` objects can be seen through the `summary` method:

``` r
summary(citations_btdata)
#> Number of players: 4 
#> Density of wins matrix: 1 
#> Fully-connected: TRUE
summary(toy_btdata)
#> Number of players: 8 
#> Density of wins matrix: 0.203125 
#> Fully-connected: FALSE 
#> Number of fully-connected components: 3 
#> Summary of fully-connected components: 
#>   Component size Freq
#> 1              1    1
#> 2              3    1
#> 3              4    1
```

Note that components of size 1 will be filtered out in the MLE fit (see next section); the model doesn't make sense for them.

`select_components` can be used to create a subset of a `btdata` object. In our toy case, the following all give the same subset:

``` r
toy_btdata_subset <- select_components(toy_btdata, "3")
toy_btdata_subset <- select_components(toy_btdata, function(x) length(x) == 4)
toy_btdata_subset <- select_components(toy_btdata, function(x) "c" %in% x)
summary(toy_btdata_subset)
#> Number of players: 4 
#> Density of wins matrix: 0.25 
#> Fully-connected: TRUE
```

Alternatively, set the `subset` argument in `btfit`.

Fitting the model
=================

The summary of the `btdata` object reveals whether the comparison graph is fully-connected or not. This affects the type of estimate available for the strength parameter.

-   If `Fully-connected: TRUE`, then the maximum likelihood estimate (MLE) for the Bradley-Terry model exists and is finite.
-   If `Fully-connected: FALSE`, then we cannot find the MLE for the full dataset. There are two alternatives:
    -   Find the MLE on each fully connected component of the comparison graph (and note then that it is only meaningful to rank and compare items within the same component).
    -   Place a Bayesian prior on the model, then find the maximum a posteriori (MAP) estimate. This always exists and is finite. It gives a principled way of ranking and comparing all *K* items.

The function `btfit` requires two arguments: the data (in the form of a `btdata` oject), and `a`, which controls whether the MLE or MAP estimate is returned.

-   If `a = 1`, the MLE is returned, either on the full dataset if the comparison graph is fully-connected, or else the MLE is found separately for each fully-connected component.
-   If `a > 1`, the MAP estimate is returned, with `a` as the value of the shape parameter in the prior.

See `?btfit` or the vignette for more details.

``` r
citations_fit <- btfit(citations_btdata, 1)
toy_fit_MLE <- btfit(toy_btdata, 1)
toy_fit_MAP <- btfit(toy_btdata, 1.1)
```

`btfit` objects are a list, and they are not designed to be examined directly, but to be passed to other methods.

Methods for a `btfit` object
============================

`summary.btift`, `coef.btfit` and `vcov.btfit`
----------------------------------------------

The `summary.btfit` method returns a list with

-   `call`: the call to to `btfit`
-   `item_summary`: a data frame with one row for each item in the fit (note that this can be fewer than the number of items in the data, if there were any components of size one, or if the fit was on a subset). Items are ranking in descending order *within each component*
-   `component_summary`: a data frame with one row per component in the fit.

The standard errors are *not* returned by default (since the underlying `vcov.btfit` function can be slow for large matrices), but can be included by setting `SE = TRUE`. It is also possible to set a reference item, and to return the summary for only a subset of components (see `?summary.btfit`).

The `coef.btfit` method extracts the parameter estimates. This is the strength parameter, on the log scale, contrainted such that the mean of the estimates is zero. By default a vector if `btfit` was run on the full-dataset or a list of vectors otherwise, but there is also the possibility of returning a data frame by setting `as_df = TRUE`.

The `vcov.btfit` method returns the variance-covariance matrix (or a list of these matrices by component), and also has `ref` and `subset` arguments (see `?vcov.btfit`).

``` r
summary(citations_fit)
#> $call
#> btfit(btdata = citations_btdata, a = 1)
#> 
#> $item_summary
#> # A tibble: 4 x 3
#>      component         item   estimate
#>          <chr>        <chr>      <dbl>
#> 1 full_dataset       JRSS-B  1.0604065
#> 2 full_dataset   Biometrika  0.7897537
#> 3 full_dataset         JASA  0.3095638
#> 4 full_dataset Comm Statist -2.1597241
#> 
#> $component_summary
#> # A tibble: 1 x 4
#>      component num_items iters converged
#>          <chr>     <int> <int>     <lgl>
#> 1 full_dataset         4     2      TRUE
summary(toy_fit_MLE, SE = TRUE)
#> $call
#> btfit(btdata = toy_btdata, a = 1)
#> 
#> $item_summary
#> # A tibble: 7 x 4
#>   component  item   estimate        SE
#>       <chr> <chr>      <dbl>     <dbl>
#> 1         2     h  1.2643415 0.7169719
#> 2         2     g -0.2488214 0.5045346
#> 3         2     f -1.0155201 0.5089739
#> 4         3     d  0.5843254 1.0020127
#> 5         3     a  0.3030107 1.0320999
#> 6         3     b -0.2987148 1.0314304
#> 7         3     c -0.5886212 1.0025948
#> 
#> $component_summary
#> # A tibble: 2 x 4
#>   component num_items iters converged
#>       <chr>     <int> <int>     <lgl>
#> 1         2         3    13      TRUE
#> 2         3         4    16      TRUE
coef(toy_fit_MAP)
#>          e          h          a          d          g          b 
#>  2.6768420  1.1322378 -0.1472393 -0.1621301 -0.3867808 -0.8484833 
#>          f          c 
#> -1.1214999 -1.1429465
vcov(citations_fit, ref = "JASA")
#>               citing
#> cited               JRSS-B  Biometrika JASA Comm Statist
#>   JRSS-B       0.005323938 0.001988435    0  0.001172936
#>   Biometrika   0.001988435 0.003671841    0  0.001396292
#>   JASA         0.000000000 0.000000000    0  0.000000000
#>   Comm Statist 0.001172936 0.001396292    0  0.009638953
```

`btprob` and `fitted.btfit`
---------------------------

This calculates the Bradley-Terry probabilities that item *i* beats item *j*. By default a matrix if `btfit` was run on the full-dataset or a list of matrices otherwise, but there is also the possibility of returning a data frame by setting `as_df = TRUE`. The `fitted.btfit` method functions similarly, except it returns the expected number of wins (see `?fitted.btfit`).

``` r
btprob(citations_fit)
#> 4 x 4 sparse Matrix of class "dgCMatrix"
#>               citing
#> cited              JRSS-B Biometrika       JASA Comm Statist
#>   JRSS-B       .           0.5672532 0.67936229    0.9615848
#>   Biometrika   0.43274683  .         0.61779270    0.9502388
#>   JASA         0.32063771  0.3822073 .             0.9219605
#>   Comm Statist 0.03841516  0.0497612 0.07803945    .
fitted(toy_fit_MLE, as_df = TRUE)
#> # A tibble: 7 x 5
#>   component player1 player2      fit1      fit2
#>       <chr>   <chr>   <chr>     <dbl>     <dbl>
#> 1         2       h       g 2.4585885 0.5414115
#> 2         2       h       f 4.5359770 0.4640230
#> 3         2       g       f 5.4624506 2.5375494
#> 4         3       d       b 0.7074518 0.2925482
#> 5         3       a       b 1.2921019 0.7078981
#> 6         3       d       c 2.2910316 0.7089684
#> 7         3       a       c 0.7092268 0.2907732
```

`simulate.btfit` and `simuate_BT`
---------------------------------

There are two functions to simuate data from a Bradley-Terry model. `simulate.btfit` takes a `btfit` object *which has been fitted on one component* (either the full dataset, or a one-component subset). `simulate_BT` takes an `N` matrix (i.e. where the *i,j*-th elemenent is the number of times items *i* and *j* have been compared) a vector `pi`, the strength parameters of a Bradley-Terry model (note that `pi` *not* the same as the estimates in `coef` and `summary`, which are on the log-scale). Both functions return a `wins` matrix by default, but can also be set to return a `btdata` object.

``` r
citations_sim <- simulate(citations_fit)
set.seed(1982)
num_items <- 6
Nmatrix <- matrix(rep(2, num_items ^ 2), num_items, num_items) # 2 contests between every pair, systematic
diag(Nmatrix) <- 0
pi_vec <- exp(rnorm(num_items)/4)
tournament_sim <- simulate_BT(pi_vec, Nmatrix, nsim = 2)
```

The Bradley-Terry model
=======================

Details of the Bradley-Terry model are not presented here (due to GitHub's inability to display equations). Instead, please refer to the (soon-to-be-written) vignette.

``` r
vignette("BradleyTerryScalable", package = "BradleyTerryScalable")
```

**NOTE: this vignette isn't actually written yet! This vignette exists but is missing content**
