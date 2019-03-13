#' @title Bootstrap version of apes_logit
#' @param x The n by p design matrix for Poisson regression
#' @param y The response vector for Poisson regression of length n
#' @param mu Esimated means of each observation using a baseline model.
#' It should be a vector of length n.
#' @param k Model size to explore. If leaps was selected as the estimator,
#' then model up to size max(k) will be explored.
#' If mio was selected as the estimator, then
#' model sizes specified in k will be explored.
#' @param estimator Either "leaps" or "mio", which correspond to
#' optimisation algorithms available in
#' the leaps and bestsubset package, respectively.
#' @param time.limit The time limit for the maximum time allocated to each
#' model size model when the "mio" estimator was selected.
#' It will not affect the speed if leaps
#' @param nBoot Number of bootstrap runs
#' @param workers Number of cores, using multicores from the furr package
#' @param seed Used to set a random seed for reproducibility. Default to NULL
#' @author Kevin Wang
#' @import furrr
#' @import future
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
#' listResult = boot_apes_poisson(
#' x = x, y = y, mu = mu, k = k,
#' estimator = "leaps", nBoot = 10, workers = 1)
#' length(listResult)

boot_apes_poisson = function(x, y, mu, k,
                             estimator = "leaps",
                             time.limit = 60,
                             nBoot = 100, workers = 1, seed = NULL){

  if(!is.null(seed)){
    set.seed(seed)
  }

  listBootIndex = base::replicate(
    n = nBoot,
    expr = sample(seq_len(nrow(x)), replace = TRUE),
    simplify = FALSE)


  single_boot_apes_poisson = function(bootIndex){

    res = APES::apes_poisson(
      x = x[bootIndex,],
      y = y[bootIndex],
      mu = mu,
      k = k,
      time.limit = time.limit,
      estimator = estimator)
    return(res)
  }



  if(workers > 1){
    future::plan(strategy = future::multiprocess, workers = workers)

    result = furrr::future_map(
      .x = listBootIndex,
      .f = single_boot_apes_poisson, .progress = TRUE)

    future::plan(strategy = future::sequential)
  } else{
    result = lapply(listBootIndex, single_boot_apes_poisson)
  }

  names(result) = paste0("bootNum", 1:nBoot)

  return(result)
}

