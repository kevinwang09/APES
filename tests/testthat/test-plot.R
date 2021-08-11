set.seed(10)
n = 100
p = 10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
data = data.frame(y, x)
model = glm(y ~ ., data = data, family = "binomial")

apes_result = apes(model = model)

## Plot a single apes object
plot(apes_result)

## Plot boot_apes object
apes_result = apes(model = model, n_boot = 20)

plot(apes_result, type = "vip_tile", order = "BIC")
plot(apes_result, type = "vip_tile", order = "AIC")
plot(apes_result, type = "vip_tile", order = "BIC", categorical = TRUE)
plot(apes_result, type = "vip")
plot(apes_result, type = "path", order = "BIC")
plot(apes_result, type = "path", order = "AIC")
plot(apes_result, type = "ma")

top_vars_AIC = head(apes_var_freq(list_result = apes_result, ic = "AIC")$variables, 3)
top_vars_BIC = head(apes_var_freq(list_result = apes_result, ic = "BIC")$variables, 3)

expect_warning(as_vis(boot_apes = apes_result, model = model))
