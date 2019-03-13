context("Test bootstrap APES poisson")

set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -0.8, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rpois(n = n, lambda = exp(x %*% beta))
mu = glm.fit(x = x, y = y, family = poisson(link = "log"))$fitted.values

oneCore = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 10, workers = 1, seed = 123)
twoCore = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 10, workers = 2, seed = 123)

remove_apesTimeDiff = function(apesRes){
  res = apesRes
  res$apesTimeDiff = NULL
  return(res)
}

oneCore_withoutTime = purrr::map(oneCore, remove_apesTimeDiff)
twoCore_withoutTime = purrr::map(twoCore, remove_apesTimeDiff)

expect_identical(oneCore_withoutTime, twoCore_withoutTime)


