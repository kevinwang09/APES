#' The logit function
#' @param x numeric
#' @export
#' @examples
#' curve(logit, from = 0.1, to = 0.9)
logit = function(x){
  log(x) - log(1-x)
}
