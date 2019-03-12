context("Test variable selection frequency")

set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -0.8, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rpois(n = n, lambda = exp(x %*% beta))
mu = glm.fit(x = x, y = y, family = poisson(link = "log"))$fitted.values
listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 10)
topVariables = head(apes_var_freq(listResult = listResult, ic = "AIC")$variables, 3)

expect_equal(
  as.character(topVariables),
  c("Int", "X1", "X2")
)

