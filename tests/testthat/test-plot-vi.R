context("vi plot")
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
viPlotResult = plot_vi(listResult)
vdiffr::expect_doppelganger("Variable inclusion plot", viPlotResult$viPlot)