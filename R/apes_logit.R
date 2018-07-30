#' Perform APES-logistic regression using either the leaps or bestsubset package
#' @param x The n by p design matrix for logistic regression
#' @param y The response vector for logistic regression of length n
#' @param Pi Esimated probabilities of each observation using a baseline model. It should be a vector of length n.
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
#' y = round(runif(n, 0,1))
#' Pi = glm(y ~ x, family = "binomial")$fitted.values
#' apesLeapsResult = apes_logit(x = x, y = y, Pi = Pi, k = k, 
#'                                estimator = "leaps")
#' 
#' apesMioResult = apes_logit(x = x, y = y, Pi = Pi, k = k, 
#'                                estimator = "mio", time.limit = 5)
#'
#' all.equal(apesLeapsResult$apesMleBeta, apesMioResult$apesMleBeta)
#' 
#' 
apes_logit = function(x, y, Pi, k, estimator = "leaps", time.limit = 60){
  
  if(estimator == "leaps"){
    result = apes_leaps_logit(x = x, y = y, Pi = Pi,
                                maxK = max(k)
    )
  }
  
  
  if(estimator == "mio"){
    result = apes_mio_logit(x = x, y = y, Pi = Pi, 
                              krange = k,
                              time.limit = time.limit
    )
  }
  
  
  return(result)
}