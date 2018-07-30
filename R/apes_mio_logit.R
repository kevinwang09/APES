#' Perform APES-logistics regression using the bestsubset package
#' @param x The n by p design matrix for logistics regression
#' @param y The response vector for logistics regression of length n
#' @param Pi Esimated probability of each observation using a baseline model. It should be a vector of length n.
#' @param krange The range of model size to explore.
#' @import tibble
#' @import broom
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' krange = 1:5
#
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = round(runif(n, 0,1))
#' Pi = runif(n, 0, 1)
#' apesResult = apes_mio_logit(x = x, y = y, Pi = Pi, krange = krange)
#' names(apesResult)


apes_mio_logit = function(x,
                    y,
                    Pi,
                    krange,
                    time.limit = 100){

  variables = c("Int", colnames(x))
  x = x
  yBinom = y
  ###### Begin setting up linear regression #######
  n = nrow(x)
  p = ncol(x)
  linearY = log(Pi/(1-Pi)) + (yBinom-Pi)/(Pi*(1-Pi))
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
    refittingMle_logit(indicator = indicator, X = x, yBinom = yBinom)
  }) ## Each model will be different


  apesMleBeta = mleModelToBeta(mleModels = apesMleModels, variables = variables)
  colnames(apesMleBeta) = apesModelName

  apesMlePi = apply(apesMleBeta, 2, function(beta){beta2Pi(X = x, beta = beta)})
  apesMleLoglike = apply(apesMlePi, 2, function(mlePi){loglikePi(yBinom = yBinom, pis = mlePi)})
  apesMleBIC = -2*apesMleLoglike + log(n)*apesModelSize
  apesMleAIC = -2*apesMleLoglike + 2*apesModelSize

  ############# End Exact Logistic solver ##############

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
  colnames(apesMlePi) = apesModelName
  rownames(apesMlePi) = obsNum

  ############
  apesMinAicProb = minIcMatrix(ic = apesMleAIC,
                               mat = apesMlePi)
  apesMinBicProb = minIcMatrix(ic = apesMleBIC,
                               mat = apesMlePi)
  ############
  responseTibble = data.frame(obsNum = obsNum,
                              y = y,
                              Pi = Pi,
                              linearY = linearY,
                              apesMinAicProb = apesMinAicProb,
                              apesMinBicProb = apesMinBicProb) %>%
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
