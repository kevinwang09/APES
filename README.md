
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APES: APproximated Exhaustive Search for GLMs

[![R build
status](https://github.com/kevinwang09/APES/workflows/R-CMD-check/badge.svg)](https://github.com/kevinwang09/APES/actions)
[![Coverage
status](https://codecov.io/gh/kevinwang09/APES/branch/master/graph/badge.svg)](https://codecov.io/github/kevinwang09/APES?branch=master)

<img src="inst/APES_logo.png" align="right" width="200" />

APproximated Exhaustive Search (APES) is a model selection method for
Generalised Linear Models. The accompanying paper is [Wang et.
al. (2019)](https://doi.org/10.1111/anzs.12276). You can find the
vignette [here](https://kevinwang09.github.io/APES/articles/APES.html).

## Installation

``` r
library(devtools)
devtools::install_github("kevinwang09/APES")
```

## A quick example

Suppose we have a data with 500 rows and 20 variables, and we have
fitted a logistic regression model. We may wish to perform an exhaustive
variable selection on such a model to determine which variables produce
the most parsimonious model. However, performing an exhaustive variable
selection means looking through 2^20 = 1,048,576 models\! Exhaustive
variable selection is known to be time consuming, and this might take a
long time.

APES is a variable selection method that first converts the logistic
model into a linear model and then it uses a best-subset algorithm (such
as leaps or mixed integer optimisation) to search for the best linear
model. The selected linear models are then converted into logistic
models. The reason for doing this is that the exhaustive variable
selection can be performed much faster in the linear model space.

The current implementation of APES supports logistic, poisson and Cox
regression models.

``` r
library(APES)

## Simulating data
set.seed(10)
n = 500
p = 20
k = 1:p
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
data = data.frame(y, x)

## Fitting a full model 
model = glm(y ~ ., data = data, family = "binomial")

## Running APES selection
apes_result = apes(model = model)
apes_result
#> Time taken: 
#> Time difference of 0.0001144012 mins
#> 
#>  APES model selection data frame: 
#> # A tibble: 20 x 7
#>    model_name  model_size ic_opt_models apes_mle_loglike mle_aic mle_bic status 
#>    <chr>            <dbl> <chr>                    <dbl>   <dbl>   <dbl> <chr>  
#>  1 apes_model…          2 ""                       -297.    599.    607. leaps_…
#>  2 apes_model…          3 "apes_min_bi…            -270.    546.    558. leaps_…
#>  3 apes_model…          4 ""                       -269.    545.    562. leaps_…
#>  4 apes_model…          5 ""                       -267.    544.    565. leaps_…
#>  5 apes_model…          6 ""                       -266.    545.    570. leaps_…
#>  6 apes_model…          7 ""                       -266.    545.    575. leaps_…
#>  7 apes_model…          8 ""                       -263.    543.    577. leaps_…
#>  8 apes_model…          9 ""                       -262.    542.    580. leaps_…
#>  9 apes_model…         10 ""                       -261.    543.    585. leaps_…
#> 10 apes_model…         11 "apes_min_ai…            -260.    541.    587. leaps_…
#> 11 apes_model…         12 ""                       -260.    543.    594. leaps_…
#> 12 apes_model…         13 ""                       -258.    543.    598. leaps_…
#> 13 apes_model…         14 ""                       -258.    544.    603. leaps_…
#> 14 apes_model…         15 ""                       -257.    544.    608. leaps_…
#> 15 apes_model…         16 ""                       -257.    546.    614. leaps_…
#> 16 apes_model…         17 ""                       -257.    548.    620. leaps_…
#> 17 apes_model…         18 ""                       -257.    550.    626. leaps_…
#> 18 apes_model…         19 ""                       -257.    551.    631. leaps_…
#> 19 apes_model…         20 ""                       -257.    553.    638. leaps_…
#> 20 apes_model…         21 ""                       -256.    555.    643. leaps_…
apes_result$selected_model_beta
#>           apes_min_aic apes_min_bic
#> intercept   -0.1606897   -0.1920528
#> X1           1.0891026    1.0773367
#> X2          -0.8247133   -0.8352282
#> X3           0.0000000    0.0000000
#> X4           0.1467194    0.0000000
#> X5           0.0000000    0.0000000
#> X6           0.0000000    0.0000000
#> X7           0.0000000    0.0000000
#> X8           0.1607424    0.0000000
#> X9           0.1913598    0.0000000
#> X10          0.0000000    0.0000000
#> X11         -0.1001883    0.0000000
#> X12          0.0000000    0.0000000
#> X13          0.1869966    0.0000000
#> X14          0.0000000    0.0000000
#> X15          0.0000000    0.0000000
#> X16          0.0000000    0.0000000
#> X17          0.2199132    0.0000000
#> X18          0.2186733    0.0000000
#> X19          0.0000000    0.0000000
#> X20         -0.2280918    0.0000000
```

The best model of each model size are stored and the best model of all
can be selected using an information criterion such as the Akaike
Information Criterion (AIC) or the Bayesian Information Criterion (BIC).
Notice how in the above output of APES, the true model size is
identified as 3 and 2 by the AIC and BIC respectively. The selected
model estimates are also shown.

The bootstrap procedure can be used to understand the variable selection
stability. The APES package also comes with some plotting functions that
displays the bootstrapped results.

``` r
boot_result = apes(model = model, n_boot = 20)
boot_result
#> Time taken:  0.03168368  minutes 
#> Total number of bootstrap APES results:  20
plot(boot_result, type = "vip")
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

# References

  - Wang, K. Y., Tarr, G., Yang, J. Y., & Mueller, S. (2019). Fast and
    approximate exhaustive variable selection for generalised linear
    models with APES. Australian & New Zealand Journal of Statistics,
    61(4), 445–465. <https://doi.org/10.1111/anzs.12276>
  - Icon made by Freepik from www.flaticon.com
