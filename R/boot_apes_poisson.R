#' Bootstrap version of apes_logit
#' @title Bootstrap version of apes_logit
#' @author Kevin Wang
#' @import parallel
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' k = 1:10
#' beta = c(1, -1, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rpois(n = n, lambda = exp(x %*% beta))
#' mu = glm.fit(x = x, y = y, family = poisson(link = "log"))$fitted.values
#' listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 10)
#' length(listResult)


boot_apes_poisson = function(x, y, mu, k,
                           estimator = "leaps",
                           time.limit = 60,
                           nBoot = 100, nCores = 1){
  if(nCores > 1){
    result = parallel::mclapply(1:nBoot, function(thisLoop){
      bootSample = sample(1:nrow(x), nrow(x), replace = TRUE)
      res = APES::apes_poisson(
        x = x[bootSample,],
        y = y[bootSample],
        mu = mu,
        k = k,
        time.limit = time.limit,
        estimator = estimator)
      return(res)
    }, mc.cores = nCores)
  } else{
    result = lapply(1:nBoot, function(thisLoop){
      bootSample = sample(1:nrow(x), nrow(x), replace = TRUE)
      res = APES::apes_poisson(
        x = x[bootSample,],
        y = y[bootSample],
        mu = mu,
        k = k,
        time.limit = time.limit,
        estimator = estimator)
      return(res)
    })
  }

  names(result) = paste0("bootNum", 1:nBoot)

  return(result)
}
