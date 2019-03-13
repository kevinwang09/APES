context("BIC plot")

set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rpois(n = n, lambda = exp(x %*% beta))
mu = glm.fit(x = x, y = y, family = poisson(link = "log"))$fitted.values

listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 20)
aic_plot <- plot_apes_ic(listResult, type = "AIC")
bic_plot <- plot_apes_ic(listResult, type = "BIC")
# vdiffr::manage_cases()
vdiffr::expect_doppelganger("APES BIC path plot", bic_plot)
vdiffr::expect_doppelganger("APES AIC path plot", aic_plot)


