#' The expit function
#' @param x numeric
#' @export
#' @example curve(expit, from = -5, to = 5)
expit = function(x){
  1/(1+exp(-x))
}
