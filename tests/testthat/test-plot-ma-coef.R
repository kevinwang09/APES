context("MA coef plot")

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

list_result = apes(model = model, n_boot = 20)

aic_plot <- plot_ma_coef(list_result = list_result, type = "AIC")
bic_plot <- plot_ma_coef(list_result = list_result, type = "BIC")
# vdiffr::manage_cases()
vdiffr::expect_doppelganger("Cumulative MA coefficients using AIC", aic_plot)
vdiffr::expect_doppelganger("Cumulative MA coefficients using BIC", bic_plot)

