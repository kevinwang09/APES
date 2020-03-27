
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APES: APproximated Exhaustive Search for GLMs

[![Travis build
status](https://travis-ci.org/kevinwang09/APES.svg?branch=master)](https://travis-ci.org/kevinwang09/APES)
[![Coverage
status](https://codecov.io/gh/kevinwang09/APES/branch/master/graph/badge.svg)](https://codecov.io/github/kevinwang09/APES?branch=master)

<img src="inst/APES_logo.png" align="right" width="200" />

APproximated Exhaustive Search (APES) for Generalised Linear Models is a
model selection method to be published by Kevin YX. Wang, Garth Tarr,
Jean YH Yang and Samuel Mueller at the University of Sydney.

This is a repository for the R package of APES. It will be eventually be
integrated into the mplot package
(<https://github.com/garthtarr/mplot>).

The accompanying paper is provisionally accepted by Australia and New
Zealand Journal of Statistics.

## Installation

``` r
library(devtools)
devtools::install_github("kevinwang09/APES")
```

## A quick example

Suppose we have a data with 100 rows and 10 variables, and we wish to
perform an exhaustive variable selection using the classical logistic
model. Exhaustive variable selection is known to be time consuming, so
this might take a long time. APES is a variable selection method that
first converts the logistic model into a linear model first and then
uses a best-subset algorithm (such as leaps or mixed integer
optimisation) to search for the best linear model to obtain the best
variables. These selected variables in the linear space then represents
the best model for the original logistic model.

The application of APES is not restricted to logistic model but to all
GLMs.

``` r
library(APES)

set.seed(10)
n = 100
p = 10
k = 1:p
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
Pi = glm.fit(x = x, y = y, family = binomial(link = "logit"))$fitted.values
apesLeapsResult = apes_logit(x = x, y = y, Pi = Pi, k = k,
                               estimator = "leaps")
#> [1] "Finished solving linear regression approximation"
apesLeapsResult$apesModelDf
#> # A tibble: 10 x 8
#>    method modelName modelSize apesMleLoglike mleAIC mleBIC status
#>    <chr>  <chr>         <dbl>          <dbl>  <dbl>  <dbl> <chr> 
#>  1 apes   apesMode…         2          -59.4   123.   128. leaps…
#>  2 apes   apesMode…         3          -53.3   113.   120. leaps…
#>  3 apes   apesMode…         4          -51.8   112.   122. leaps…
#>  4 apes   apesMode…         5          -51.2   112.   125. leaps…
#>  5 apes   apesMode…         6          -50.6   113.   129. leaps…
#>  6 apes   apesMode…         7          -50.1   114.   132. leaps…
#>  7 apes   apesMode…         8          -50.1   116.   137. leaps…
#>  8 apes   apesMode…         9          -50.1   118.   142. leaps…
#>  9 apes   apesMode…        10          -50.0   120.   146. leaps…
#> 10 apes   apesMode…        11          -49.9   122.   151. leaps…
#> # … with 1 more variable: icOptimalModels <chr>
```

# References

  - Wang, K. Y., Tarr, G., Yang, J. Y., & Mueller, S. (2019). Fast and
    approximate exhaustive variable selection for generalised linear
    models with APES. Australian & New Zealand Journal of Statistics,
    61(4), 445–465. <https://doi.org/10.1111/anzs.12276>
  - Icon made by Freepik from www.flaticon.com
