set.seed(10)
n = 200
p = 10
k = 1:10
beta = c(1, -1, rep(0, p-2))
x = matrix(rnorm(n*p), ncol = p)
colnames(x) = paste0("X", 1:p)

hx = exp(x %*% beta)
time = rexp(n, hx)
censor = rbinom(n = n, prob = 0.3, size = 1) # censoring indicator
data = data.frame(x, time = time, censor = censor)
model = survival::coxph(survival::Surv(time, censor) ~ ., data = data)
apes_result = apes(model = model)
apes_result
