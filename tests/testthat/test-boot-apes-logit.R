context("Test bootstrap APES logit")

set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
Pi = glm.fit(x = x, y = y, family = binomial(link = "logit"))$fitted.values
apesLeapsResult = apes_logit(x = x, y = y, Pi = Pi, k = k,
                             estimator = "leaps")

oneCore = boot_apes_logit(x = x, y = y, Pi = Pi, k = k, estimator = "leaps", nBoot = 10, workers = 1, seed = 123)
twoCore = boot_apes_logit(x = x, y = y, Pi = Pi, k = k, estimator = "leaps", nBoot = 10, workers = 2, seed = 123)

remove_apesTimeDiff = function(apesRes){
  res = apesRes
  res$apesTimeDiff = NULL
  return(res)
}

oneCore_withoutTime = purrr::map(oneCore, remove_apesTimeDiff)
twoCore_withoutTime = purrr::map(twoCore, remove_apesTimeDiff)

expect_identical(oneCore_withoutTime, twoCore_withoutTime)

oneCore_secondRun = boot_apes_logit(x = x, y = y, Pi = Pi, k = k, estimator = "leaps", nBoot = 10, workers = 1, seed = 123)
expect_identical(
  purrr::map(oneCore, "responseTibble"),
  purrr::map(oneCore_secondRun, "responseTibble")
)
