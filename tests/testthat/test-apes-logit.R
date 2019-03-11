context("Test APES logit")

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

testthat::expect_equal(which.min(apesLeapsResult$apesModelDf$mleBIC), 2)


if("bestsubset" %in% rownames(installed.packages())){
  apesMioResult = apes_logit(x = x, y = y, Pi = Pi, k = k,
                             estimator = "mio", time.limit = 5)
  testthat::expect_equal(
    apesLeapsResult$apesMleBeta, apesMioResult$apesMleBeta
  )
} 

######################## Test for warnings on degenerate Pi #############
set.seed(10)
beta = c(7.5, rep(0, p-1))
yDegen = rbinom(n = n, size = 1, prob = expit(x %*% beta))
PiDegen = suppressWarnings(
  glm.fit(x = x, y = yDegen, family = binomial(link = "logit"))$fitted.values
) ## Suppose the users did not see the wanring

hist(PiDegen)

testthat::expect_warning(
  apes_logit(x = x, y = yDegen, Pi = PiDegen, k = k,
             estimator = "leaps")
)