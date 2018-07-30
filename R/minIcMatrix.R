#' Not to be used by end-users
#' @param ic numeric vector, information criterion
#' @param mat matrix

minIcMatrix = function(ic, mat){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = mat[,which.min(ic)]
  }
  return(res)
}
