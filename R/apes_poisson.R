#' Perform APES-Poisson regression using either the leaps or bestsubset package
#' @param x The n by p design matrix for Poisson regression
#' @param y The response vector for Poisson regression of length n
#' @param mu Esimated means of each observation using a baseline model. It should be a vector of length n.
#' @param k Model size to explore. If leaps was selected as the estimator, then model up to size max(k) will be explored. If mio was selected as the estimator, then model sizes specified in k will be explored. 
#' @param estimator Either "leaps" or "mio", which correspond to optimisation algorithms available in the leaps and bestsubset package, respectively. 
#' @param time.limit The time limit for the maximum time allocated to each model size model when the "mio" estimator was selected. It will not affect the speed if leaps
#' @import leaps
#' @import tibble
#' @import broom
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#'
#' set.seed(10)
#' n = 100
#' p = 10
#' k = 1:5
#
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rpois(n, 5)
#' mu = glm(y ~ x,family = "poisson")$fitted.value
#' apesLeapsResult = apes_poisson(x = x, y = y, mu = mu, k = k, 
#'                                estimator = "leaps")
#' 
#' apesMioResult = apes_poisson(x = x, y = y, mu = mu, k = k, 
#'                                estimator = "mio", time.limit = 5)
#'
#' all.equal(apesLeapsResult$apesMleBeta, apesMioResult$apesMleBeta)
#' 
apes_poisson = function(x, y, mu, k, estimator = "leaps", time.limit = 60){
  
  if(estimator == "leaps"){
    result = apes_leaps_poisson(x = x, y = y, mu = mu, 
                                maxK = max(k)
                                )
  }
  
  
  if(estimator == "mio"){
    result = apes_mio_poisson(x = x, y = y, mu = mu, 
                              krange = k,
                              time.limit = time.limit
    )
  }
  
  
  return(result)
}