
<!-- README.md is generated from README.Rmd. Please edit that file -->
BradleyTerryScalable
====================

[![Travis-CI Build Status](https://travis-ci.org/EllaKaye/BradleyTerryScalable.svg?branch=master)](https://travis-ci.org/EllaKaye/BradleyTerryScalable) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/BradleyTerryScalable)](https://cran.r-project.org/package=BradleyTerryScalable)

An R package for fitting the Bradley-Terry model to pair-comparison data, to enable statistically principled ranking of a potentially large number of objects.

Given a number of items for whom we have pair-comparison data, the Bradley-Terry model assigns a 'strength' parameter to each item. These can be used to rank the items. Moreover, they can be used to determine the probability that any given item will 'beat' any other given item when they are compared. Further details of the mathematical model, and the algorithms used to fit them, will soon be available in a vignette (see the end of this `README` for vignette details).

**Note: this package is currently in the final stages of development. Some changes are likely in the user interface and documentation, especially, prior to the package's scheduled initial submission to CRAN in late June 2017.**

**Documentation website can be found [here](https://ellakaye.github.io/BradleyTerryScalable/) (thanks to [`pkgdown`](https://github.com/hadley/pkgdown))**

Installing the package
----------------------

``` r
# install.packages("devtools")
devtools::install_github("EllaKaye/BradleyTerryScalable", build_vignettes = TRUE)
```

``` r
library(BradleyTerryScalable)
```

Preparing the data
------------------

The main fitting function in this package is `btfit`. This function takes as its main argument an object of class `btdata`. To create a `btdata` object, use the function `btdata(x)`.

The `x` argument to `btdata` can be one of four classes of object:

-   A data frame (`data.frame` or `tibble`), with three or four columns
    -   If the data frame has three columns, then the first column must be the name of item 1, the second column must be the name of item 2, and the third column must be the number of times item 1 has beaten item 2.
    -   If the data frame has four columns, then the first column must be the name of item 1, the second column must be the name of item 2, and the third column must be the number of times item 1 has beaten item 2 and the fourth column must be the number of times item 2 has beaten item 1.
-   A matrix (either a base `matrix` or a class from the `Matrix` package), dimension K by K, where K is the number of items. The *i,j*-th element is the number of times item *i* has beaten item *j*.

-   A contingency table of class `table`, similar to the matrix described in the above point.

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
#>    player1 player2 outcome
#> 1      Cyd     Amy      W1
#> 2      Amy     Ben       D
#> 3      Ben     Eve      W2
#> 4      Cyd     Dan      W2
#> 5      Ben     Dan       D
#> 6      Dan     Eve      W2
#> 7      Fin     Eve      W2
#> 8      Fin     Gal      W2
#> 9      Fin     Han      W2
#> 10     Eve     Gal      W1
#> 11     Fin     Gal       D
#> 12     Han     Gal      W1
#> 13     Han     Gal      W2
#> 14     Amy     Dan      W1
#> 15     Cyd     Amy      W1
#> 16     Ben     Dan       D
#> 17     Dan     Amy      W2
```

Run `btdata()` to produce objects of class `btdata`:

``` r
citations_btdata <- btdata(citations)
toy_data_4col <- codes_to_counts(toy_data, c("W1", "W2", "D"))
toy_btdata <- btdata(toy_data_4col, return_graph = TRUE) 
```

`btdata` objects are a list containing two elements:

-   `wins` which is a matrix of the form described in the second bullet point above
-   `components` which is itself a list of the fully-connected components of the comparison graph (see the third bullet point above)

Information about the `btdata` objects can be seen through the `summary` method:

``` r
summary(citations_btdata)
#> Number of items: 4 
#> Density of wins matrix: 1 
#> Fully-connected: TRUE
summary(toy_btdata)
#> Number of items: 8 
#> Density of wins matrix: 0.25 
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
toy_btdata_subset <- select_components(toy_btdata, function(x) "Cyd" %in% x)
summary(toy_btdata_subset)
#> Number of items: 4 
#> Density of wins matrix: 0.4375 
#> Fully-connected: TRUE
```

Alternatively, set the `subset` argument in `btfit`.

Fitting the model
-----------------

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
----------------------------

### `summary.btift`, `coef.btfit` and `vcov.btfit`

The `summary.btfit` method returns a list with

-   `call`: the call to `btfit`
-   `item_summary`: a data frame with one row for each item in the fit (note that this can be fewer than the number of items in the data, if there were any components of size one, or if the fit was on a subset). Items are ranking in descending order *within each component*
-   `component_summary`: a data frame with one row per component in the fit.

The standard errors are *not* returned by default (since the underlying `vcov.btfit` function can be slow for large matrices), but can be included by setting `SE = TRUE`. It is also possible to set a reference item, and to return the summary for only a subset of components (see `?summary.btfit`).

The `coef.btfit` method extracts the parameter estimates. This is the strength parameter, on the log scale, constrained such that the mean of the estimates is zero. By default a vector if `btfit` was run on the full-dataset or a list of vectors otherwise, but there is also the possibility of returning a data frame by setting `as_df = TRUE`.

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
#>   component  item    estimate        SE
#>       <chr> <chr>       <dbl>     <dbl>
#> 1         2   Han  0.69564153 0.9110076
#> 2         2   Gal  0.41253614 0.7675914
#> 3         2   Fin -1.10817768 1.0499275
#> 4         3   Cyd  0.59239992 0.9905958
#> 5         3   Amy  0.03250119 0.6990438
#> 6         3   Ben -0.24307179 0.9443103
#> 7         3   Dan -0.38182932 0.7124240
#> 
#> $component_summary
#> # A tibble: 2 x 4
#>   component num_items iters converged
#>       <chr>     <int> <int>     <lgl>
#> 1         2         3     6      TRUE
#> 2         3         4    10      TRUE
coef(toy_fit_MAP)
#>         Eve         Cyd         Han         Amy         Gal         Ben 
#>  1.90113420  0.47237293  0.24535391 -0.07655328 -0.10175687 -0.42296697 
#>         Dan         Fin 
#> -0.53638389 -1.48120003
vcov(citations_fit, ref = "JASA")
#>               citing
#> cited               JRSS-B  Biometrika JASA Comm Statist
#>   JRSS-B       0.005323938 0.001988435    0  0.001172936
#>   Biometrika   0.001988435 0.003671841    0  0.001396292
#>   JASA         0.000000000 0.000000000    0  0.000000000
#>   Comm Statist 0.001172936 0.001396292    0  0.009638953
```

### `btprob` and `fitted.btfit`

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
#> # A tibble: 8 x 5
#>   component player1 player2      fit1      fit2
#>       <chr>   <chr>   <chr>     <dbl>     <dbl>
#> 1         2     Han     Gal 1.1406148 0.8593852
#> 2         2     Han     Fin 0.8586132 0.1413868
#> 3         2     Gal     Fin 1.6412871 0.3587129
#> 4         3     Cyd     Amy 1.2728582 0.7271418
#> 5         3     Amy     Ben 0.5684605 0.4315395
#> 6         3     Cyd     Dan 0.7259617 0.2740383
#> 7         3     Amy     Dan 1.2042516 0.7957484
#> 8         3     Ben     Dan 1.0692677 0.9307323
```

### `simulate.btfit` and `simuate_BT`

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
-----------------------

Details of the Bradley-Terry model are not presented here (due to GitHub's inability to display equations). Instead, please refer to the vignette.

``` r
vignette("BradleyTerryScalable", package = "BradleyTerryScalable")
```
