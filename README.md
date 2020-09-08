
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

Suppose we have a data with 100 rows and 20 variables, and we have
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

set.seed(10)
n = 1000
p = 20
k = 1:p
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)

y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
data = data.frame(y, x)
model = glm(y ~ ., data = data, family = "binomial")
summary(model)
#> 
#> Call:
#> glm(formula = y ~ ., family = "binomial", data = data)
#> 
#> Deviance Residuals: 
#>     Min       1Q   Median       3Q      Max  
#> -3.1502  -0.8479   0.2829   0.8797   2.2778  
#> 
#> Coefficients:
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.12740    0.07635   1.669   0.0952 .  
#> X1           1.04245    0.09097  11.460   <2e-16 ***
#> X2          -1.04108    0.08898 -11.700   <2e-16 ***
#> X3          -0.01534    0.07520  -0.204   0.8383    
#> X4           0.01154    0.07347   0.157   0.8752    
#> X5           0.03467    0.07388   0.469   0.6389    
#> X6          -0.02448    0.07435  -0.329   0.7419    
#> X7           0.07472    0.07873   0.949   0.3425    
#> X8           0.06330    0.07423   0.853   0.3937    
#> X9           0.08220    0.07613   1.080   0.2802    
#> X10          0.03663    0.07932   0.462   0.6442    
#> X11         -0.13036    0.07409  -1.760   0.0785 .  
#> X12          0.01714    0.07661   0.224   0.8230    
#> X13          0.05608    0.07619   0.736   0.4617    
#> X14          0.01551    0.07869   0.197   0.8438    
#> X15         -0.04453    0.07605  -0.586   0.5582    
#> X16          0.03157    0.07405   0.426   0.6698    
#> X17          0.12395    0.07826   1.584   0.1132    
#> X18          0.08298    0.07408   1.120   0.2627    
#> X19         -0.03889    0.07473  -0.520   0.6028    
#> X20         -0.14015    0.07754  -1.807   0.0707 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> (Dispersion parameter for binomial family taken to be 1)
#> 
#>     Null deviance: 1384.7  on 999  degrees of freedom
#> Residual deviance: 1054.9  on 979  degrees of freedom
#> AIC: 1096.9
#> 
#> Number of Fisher Scoring iterations: 4
apes(model = model)
#> Time taken: 
#> Time difference of 4.553397e-05 mins
#> 
#>  APES model selection data frame: 
#> # A tibble: 20 x 8
#>    method model_name model_size apes_mle_loglike mle_aic mle_bic status
#>    <chr>  <chr>           <dbl>            <dbl>   <dbl>   <dbl> <chr> 
#>  1 apes   apes_mode…          2            -622.   1248.   1258. leaps…
#>  2 apes   apes_mode…          3            -535.   1077.   1091. leaps…
#>  3 apes   apes_mode…          4            -533.   1074.   1094. leaps…
#>  4 apes   apes_mode…          5            -533.   1076.   1101. leaps…
#>  5 apes   apes_mode…          6            -533.   1078.   1107. leaps…
#>  6 apes   apes_mode…          7            -533.   1080.   1114. leaps…
#>  7 apes   apes_mode…          8            -533.   1081.   1120. leaps…
#>  8 apes   apes_mode…          9            -531.   1080.   1124. leaps…
#>  9 apes   apes_mode…         10            -530.   1081.   1130. leaps…
#> 10 apes   apes_mode…         11            -530.   1083.   1137. leaps…
#> 11 apes   apes_mode…         12            -530.   1084.   1143. leaps…
#> 12 apes   apes_mode…         13            -530.   1086.   1150. leaps…
#> 13 apes   apes_mode…         14            -530.   1088.   1157. leaps…
#> 14 apes   apes_mode…         15            -530.   1090.   1163. leaps…
#> 15 apes   apes_mode…         16            -530.   1091.   1170. leaps…
#> 16 apes   apes_mode…         17            -528.   1091.   1174. leaps…
#> 17 apes   apes_mode…         18            -528.   1091.   1180. leaps…
#> 18 apes   apes_mode…         19            -528.   1093.   1187. leaps…
#> 19 apes   apes_mode…         20            -527.   1095.   1193. leaps…
#> 20 apes   apes_mode…         21            -527.   1097.   1200. leaps…
#> # … with 1 more variable: ic_opt_models <chr>
```

The best model of each model size are stored and the best model of all
can be selected using an information criterion such as the Akaike
Information Criterion or the Bayesian Information Criterion.

# References

  - Wang, K. Y., Tarr, G., Yang, J. Y., & Mueller, S. (2019). Fast and
    approximate exhaustive variable selection for generalised linear
    models with APES. Australian & New Zealand Journal of Statistics,
    61(4), 445–465. <https://doi.org/10.1111/anzs.12276>
  - Icon made by Freepik from www.flaticon.com
