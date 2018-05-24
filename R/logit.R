#' The expit function
#' @param x numeric
#' @export

logit = function(x){
  log(x) - log(1-x)
}
