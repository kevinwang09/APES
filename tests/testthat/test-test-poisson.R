set.seed(10)
n = 100
p = 10
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)
y = rpois(n = n, lambda = exp(x %*% beta))
data = data.frame(y, x)
model = glm(y ~ ., data = data, family = "poisson")

apes_result = apes(model = model)
print(apes_result)

boot_result = apes(model = model, n_boot = 10)
print(boot_result)
summary(boot_result)
