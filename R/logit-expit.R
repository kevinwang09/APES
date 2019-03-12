#' @title The logit function
#' @param x numeric
#' @export
#' @examples
#' curve(logit, from = 0.1, to = 0.9)
logit = function(x){
  if(any(x <= 0) | any(x >= 1)){
    error("x must be strictly between zero and 1")
  }

  return(log(x) - log(1-x))
}


#' @title The expit function
#' @param x numeric
#' @export
#' @examples
#' curve(expit, from = -5, to = 5)
expit = function(x){
  return(1/(1+exp(-x)))
}
