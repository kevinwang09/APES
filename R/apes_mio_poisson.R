#' Perform APES-logistics regression using the bestsubset package
#' @param x The n by p design matrix for Poisson regression
#' @param y The response vector for Poisson regression of length n
#' @param mu Esimated means of each observation using a baseline model. It should be a vector of length n.
#' @param krange The range of model size to explore.
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
#' krange = 1:5
#
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rpois(n, 5)
#' mu = glm(y ~ x,family = "poisson")$fitted.value
#' apesResult = apes_mio_poisson(x = x, y = y, mu = mu, krange = krange)
#' names(apesResult)

apes_mio_poisson = function(x,
                            y,
                            mu,
                            krange,
                            time.limit = 100){
  
  variables = c("Int", colnames(x))
  x = x
  yPois = y
  ###### Begin setting up linear regression #######
  n = nrow(x)
  p = ncol(x)
  linearY = log(mu) + (1/mu)*(yPois - mu)
  ###### End setting up linear regression #######
  
  ############# Begin apes solver ##############
  apesT1 = Sys.time()
  apesRes = apesSolver_mio(x = x,
                           y = linearY,
                           # k = krange
                           k = krange,
                           time.limit = time.limit
  )
  apesT2 = Sys.time()
  print("Finished")
  ## Something is very wrong with the intercept term of apes, so we will call this version as apesHosmer
  ## We will construct apesMle based on the indicator functions
  ############# End apes solver ##############
  
  ############# Begin Exact Logistic solver ##############
  
  ### For APES
  ## Authors scaled the model so that there is no intercept term of apes, so we will call this version as apesHosmer
  ## We will construct apesMle based on the indicator functions
  apesHosmerBeta = apesRes$beta
  apesIndicator = (apesHosmerBeta != 0)
  apesModelSize = colSums(apesIndicator) + 1L ## We always include intercept term!!
  apesModelName = paste0("apesModel", apesModelSize)
  
  
  apesMleModels = apply(apesIndicator, 2, function(indicator){
    refittingMle_poisson(indicator = indicator, X = x, yPois = yPois)
  }) ## Each model will be different
  
  
  apesMleBeta = mleModelToBeta(mleModels = apesMleModels, variables = variables)
  colnames(apesMleBeta) = apesModelName
  
  apesMleMu = apply(apesMleBeta, 2, function(beta){beta2Mu(X = x, beta = beta)})
  apesMleLoglike = apply(apesMleMu, 2, function(mleMu){loglikeMu(yPois = yPois, mus = mleMu)})
  apesMleBIC = -2*apesMleLoglike + log(n)*apesModelSize
  apesMleAIC = -2*apesMleLoglike + 2*apesModelSize
  
  ############# End Exact Poisson solver ##############
  
  ############# Constructing model information ##################
  apesModelDf = tibble(
    method = "apes",
    modelName = apesModelName,
    modelSize = apesModelSize,
    apesMleLoglike = apesMleLoglike,
    mleAIC = apesMleAIC,
    mleBIC = apesMleBIC,
    status = apesRes$status,
    icOptimalModels = paste0(
      icOptimal(ic = apesMleAIC, "apesMinAic"),
      icOptimal(ic = apesMleBIC, "apesMinBic")
    ) ## This variable records where the min AIC and min BIC models are.
    # l2MleDist = betaL2(apesMleBeta, beta) ## L2 error of the apesMleBeta vector with the true beta.
  )
  
  ############# Constructing Estimated Probability information ##################
  
  obsNum = paste0("obs", 1:n)
  colnames(apesMleMu) = apesModelName
  rownames(apesMleMu) = obsNum
  
  ############
  apesMinAicMean = minIcMatrix(ic = apesMleAIC,
                               mat = apesMleMu)
  apesMinBicMean = minIcMatrix(ic = apesMleBIC,
                               mat = apesMleMu)
  ############
  responseTibble = tibble::tibble(obsNum = obsNum,
                                  y = y,
                                  mu = mu,
                                  linearY = linearY,
                                  apesMinAicMean = apesMinAicMean,
                                  apesMinBicMean = apesMinBicMean) %>%
    as.tbl
  
  apesMleBetaBinary = reshape2::melt(apesMleBeta != 0,
                                     varnames = c("variables", "modelName"),
                                     value.name = "fittedBeta") %>% as.tbl
  
  selectedModelBeta = cbind(
    apesMinAic = minIcMatrix(ic = apesMleAIC, mat = apesMleBeta),
    apesMinBic = minIcMatrix(ic = apesMleBIC, mat = apesMleBeta)
  )
  
  apesTimeDiff = difftime(apesT2, apesT1, units = "mins")
  
  
  result = list(
    apesModelDf = apesModelDf,
    apesMleBeta = apesMleBeta,
    apesMleBetaBinary = apesMleBetaBinary,
    apesTimeDiff = apesTimeDiff,
    
    selectedModelBeta = selectedModelBeta,
    responseTibble = responseTibble
  )
  
  return(result)
}
