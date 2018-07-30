#' Not to be used by end-users
#' @param x Linearised APES design matrix
#' @param y Linearised APES response vector
#' @param k The range of model size to explore.
#' @param time.limit Time limit in seconds

apesSolver_mio = function(x, y, k = 1:min(nrow(x), ncol(x)), time.limit){
  apes.obj = bestsubset::bs(x = x,
                            y = y,
                            intercept = T,
                            k = k,
                            verbose = TRUE,
                            tol = 1e-6,
                            form = 1,
                            nruns = 50,
                            time.limit = time.limit)
  return(apes.obj)
}
