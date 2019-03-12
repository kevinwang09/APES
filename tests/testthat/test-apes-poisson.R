context("Test APES poisson")

set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rpois(n = n, lambda = exp(x %*% beta))
mu = glm.fit(x = x, y = y, family = poisson(link = "log"))$fitted.values
apesLeapsResult = apes_poisson(x = x, y = y, mu = mu, k = k,
                               estimator = "leaps")

testthat::expect_equal(which.min(apesLeapsResult$apesModelDf$mleBIC), 2)


if("bestsubset" %in% rownames(installed.packages())){
  apesMioResult = apes_poisson(x = x, y = y, mu = mu, k = k,
                               estimator = "mio")
  testthat::expect_equal(
    apesLeapsResult$apesMleBeta, apesMioResult$apesMleBeta
  )
}