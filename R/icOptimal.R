#' Not to be used by end-users
#' @param ic numeric vector, information criterion
#' @param symbol Labelling minimum model with a symbol

icOptimal = function(ic, symbol){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = rep("", length(ic))
    res[which.min(ic)] = symbol
  }
  return(res)
}
