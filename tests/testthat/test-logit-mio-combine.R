context("Test logit mio combine")


if("bestsubset" %in% rownames(installed.packages())){
  set.seed(10)
  n = 100
  p = 10
  k = 1:10
  beta = c(1, -1, rep(0, p-2))
  x = matrix(rnorm(n*p), ncol = p)
  colnames(x) = paste0("X", 1:p)
  y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
  Pi = glm.fit(x = x, y = y, family = binomial(link = "logit"))$fitted.values
  firstApes = apes_logit(x = x, y = y, Pi = Pi, k = 1, estimator = "mio")
  secondApes = apes_logit(x = x, y = y, Pi = Pi, k = 2, estimator = "mio")
  listApes = list(firstApes, secondApes)
  combineApes = suppressWarnings(apes_logit_mio_combine(listApes))
  
  
  twoApes = apes_logit(x = x, y = y, Pi = Pi, k = 1:2, estimator = "mio")
  
  testthat::expect_equal(
    combineApes$apesMleBeta, twoApes$apesMleBeta
  )
}