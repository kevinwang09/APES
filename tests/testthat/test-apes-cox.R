context("Test APES Cox")

set.seed(10101)
n = 200
p = 10
nzc = p/3
x = matrix(rnorm(n*p), n, p)
colnames(x) = paste0("X", 1:p)
beta = runif(round(nzc))
fx = x[,seq(nzc)] %*% beta
hx = exp(fx)
ty = rexp(n, hx)
tcens = rbinom(n = n, prob = 0.3, size = 1) # censoring indicator
time = ty
status = 1-tcens
apesLeapsResult = apes_cox(x = x, time = time, status = status, k = p,
estimator = "leaps")

testthat::expect_equal(unname(which.min(apesLeapsResult$apesModelDf$mleBIC)), round(nzc))


if("bestsubset" %in% rownames(installed.packages())){
  apesMioResult = apes_cox(x = x, time = time, status = status, k = 1:p, estimator = "mio")
  testthat::expect_equal(
    apesLeapsResult$apesMleBeta, apesMioResult$apesMleBeta
  )
}
