
<!-- README.md is generated from README.Rmd. Please edit that file -->

# APES: APproximated Exhaustive Search for GLMs

[![R build
status](https://github.com/kevinwang09/APES/workflows/R-CMD-check/badge.svg)](https://github.com/kevinwang09/APES/actions)
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
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)

y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
data = data.frame(y, x)
model = glm(y ~ ., data = data, family = "binomial")
apes(model = model)
#> $apes_model_df
#> # A tibble: 10 x 8
#>    method model_name model_size apes_mle_loglike mle_aic mle_bic status
#>    <chr>  <chr>           <dbl>            <dbl>   <dbl>   <dbl> <chr> 
#>  1 apes   apes_mode…          2            -59.4    123.    128. leaps…
#>  2 apes   apes_mode…          3            -53.3    113.    120. leaps…
#>  3 apes   apes_mode…          4            -51.8    112.    122. leaps…
#>  4 apes   apes_mode…          5            -51.2    112.    125. leaps…
#>  5 apes   apes_mode…          6            -50.6    113.    129. leaps…
#>  6 apes   apes_mode…          7            -50.1    114.    132. leaps…
#>  7 apes   apes_mode…          8            -50.1    116.    137. leaps…
#>  8 apes   apes_mode…          9            -50.1    118.    142. leaps…
#>  9 apes   apes_mode…         10            -50.0    120.    146. leaps…
#> 10 apes   apes_mode…         11            -49.9    122.    151. leaps…
#> # … with 1 more variable: ic_opt_models <chr>
#> 
#> $apes_mle_beta
#>           apes_model_2 apes_model_3 apes_model_4 apes_model_5 apes_model_6
#> intercept   -0.1837717   -0.0541247  -0.04056027  -0.02417405   0.03110599
#> X1           0.0000000    0.9097537   0.91178021   0.94363885   0.90119152
#> X2          -1.0242090   -1.1231560  -1.14496345  -1.19155438  -1.21713022
#> X3           0.0000000    0.0000000   0.00000000   0.00000000   0.00000000
#> X4           0.0000000    0.0000000   0.00000000   0.00000000   0.00000000
#> X5           0.0000000    0.0000000   0.00000000   0.00000000   0.00000000
#> X6           0.0000000    0.0000000  -0.39158670  -0.42195420  -0.47907411
#> X7           0.0000000    0.0000000   0.00000000   0.00000000   0.00000000
#> X8           0.0000000    0.0000000   0.00000000   0.00000000   0.00000000
#> X9           0.0000000    0.0000000   0.00000000  -0.31797158  -0.34747481
#> X10          0.0000000    0.0000000   0.00000000   0.00000000  -0.32596713
#>           apes_model_7 apes_model_8 apes_model_9 apes_model_10 apes_model_11
#> intercept  -0.02537646  -0.03499840 -0.035311695  -0.033665114   -0.03719786
#> X1          0.90212184   0.90188902  0.903462199   0.897326247    0.89767827
#> X2         -1.26346112  -1.25336923 -1.255116974  -1.267791328   -1.28695784
#> X3          0.00000000   0.08668873  0.085645946   0.078934792    0.09639943
#> X4          0.23862872   0.24372960  0.244554920   0.235973676    0.24004701
#> X5          0.00000000   0.00000000  0.000000000   0.000000000   -0.09331309
#> X6         -0.52805565  -0.53206755 -0.531958021  -0.522720700   -0.51654913
#> X7          0.00000000   0.00000000 -0.007390793  -0.007214945   -0.01401226
#> X8          0.00000000   0.00000000  0.000000000   0.072018505    0.08240456
#> X9         -0.35429359  -0.34056237 -0.340276500  -0.334880672   -0.33863067
#> X10        -0.35526480  -0.36409743 -0.365341478  -0.348944738   -0.35094527
#> 
#> $apes_mle_beta_binary
#> # A tibble: 110 x 3
#>    variables model_name   fitted_beta
#>    <fct>     <fct>        <lgl>      
#>  1 intercept apes_model_2 TRUE       
#>  2 X1        apes_model_2 FALSE      
#>  3 X2        apes_model_2 TRUE       
#>  4 X3        apes_model_2 FALSE      
#>  5 X4        apes_model_2 FALSE      
#>  6 X5        apes_model_2 FALSE      
#>  7 X6        apes_model_2 FALSE      
#>  8 X7        apes_model_2 FALSE      
#>  9 X8        apes_model_2 FALSE      
#> 10 X9        apes_model_2 FALSE      
#> # … with 100 more rows
#> 
#> $time_used
#> Time difference of 1.871586e-05 mins
#> 
#> $selected_model_beta
#>           apes_min_aic apes_min_bic
#> intercept  -0.04056027   -0.0541247
#> X1          0.91178021    0.9097537
#> X2         -1.14496345   -1.1231560
#> X3          0.00000000    0.0000000
#> X4          0.00000000    0.0000000
#> X5          0.00000000    0.0000000
#> X6         -0.39158670    0.0000000
#> X7          0.00000000    0.0000000
#> X8          0.00000000    0.0000000
#> X9          0.00000000    0.0000000
#> X10         0.00000000    0.0000000
#> 
#> $model_avg_beta
#>           aic_weight_coef bic_weight_coef
#> intercept   -0.0280111380   -4.975428e-02
#> X1           0.9143184235    8.989310e-01
#> X2          -1.1764903378   -1.132568e+00
#> X3           0.0043666618    1.530326e-05
#> X4           0.0322490168    4.025344e-04
#> X5          -0.0001696129   -1.670955e-08
#> X6          -0.3526317102   -1.373989e-01
#> X7          -0.0001463119   -1.319753e-07
#> X8           0.0004751834    1.326869e-07
#> X9          -0.1649975503   -2.058866e-02
#> X10         -0.0948133452   -3.703995e-03
#> 
#> $response_tibble
#> # A tibble: 100 x 4
#>    obs_num     y fitted_values linear_y
#>    <chr>   <dbl>         <dbl>    <dbl>
#>  1 obs1        0        0.820     -4.05
#>  2 obs2        0        0.285     -2.32
#>  3 obs3        1        0.389      2.12
#>  4 obs4        1        0.233      3.10
#>  5 obs5        1        0.514      2.00
#>  6 obs6        0        0.331     -2.20
#>  7 obs7        0        0.110     -3.21
#>  8 obs8        1        0.875      3.09
#>  9 obs9        0        0.0107    -5.54
#> 10 obs10       0        0.0383    -4.26
#> # … with 90 more rows
#> 
#> attr(,"class")
#> [1] "apes"
```

# References

  - Wang, K. Y., Tarr, G., Yang, J. Y., & Mueller, S. (2019). Fast and
    approximate exhaustive variable selection for generalised linear
    models with APES. Australian & New Zealand Journal of Statistics,
    61(4), 445–465. <https://doi.org/10.1111/anzs.12276>
  - Icon made by Freepik from www.flaticon.com
