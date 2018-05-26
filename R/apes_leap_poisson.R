#' Perform APES using the leaps package
#' @param x n times p design matrix for logistics regression
#' @param y response vector for logistics regression of length n
#' @param mu esimated mean of each observation using a baseline model. Of length n.
#' @param maxK maximum model size, less than p.
#' @import leaps
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
#' maxK = 5
#
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rpois(n, 5)
#' mu = glm(y ~ x,family = "poisson")$fitted.value
#' apesResult = apes_leaps_poisson(x = x, y = y, mu = mu, maxK = maxK)
#' names(apesResult)

apes_leaps_poisson = function(x,
                      y,
                      mu,
                      maxK){

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

  apesRes = leaps::regsubsets(x = x, y = linearY,
                              really.big = T,
                              nbest = 1,
                              nvmax = maxK,
                              method = "exhaustive")
  apesT2 = Sys.time()
  print("Finished")
  ## Something is very wrong with the intercept term of apes, so we will call this version as apesHosmer
  ## We will construct apesMle based on the indicator functions
  ############# End apes solver ##############

  ############# Begin Exact Logistic solver ##############

  ### For APES
  ## Authors scaled the model so that there is no intercept term of apes, so we will call this version as apesHosmer
  ## We will construct apesMle based on the indicator functions
  summaryApesRes = summary(apesRes)

  apesIndicator = t(summaryApesRes$which[, -1])
  rownames(apesIndicator) = variables[-1]
  apesModelSize = colSums(apesIndicator) ## We always include intercept term!!
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

  ############# End Exact Logistic solver ##############

  ############# Constructing model information ##################
  apesModelDf = tibble::tibble(
    method = "apes",
    modelName = apesModelName,
    modelSize = apesModelSize,
    apesMleLoglike = apesMleLoglike,
    mleAIC = apesMleAIC,
    mleBIC = apesMleBIC,
    status = "leaps_optimal",
    icOptimalModels = paste0(
      icOptimal(ic = apesMleAIC, "apesMinAic"),
      icOptimal(ic = apesMleBIC, "apesMinBic")
    ) ## This variable records where the min AIC and min BIC models are.
    # l2MleDist = betaL2(apesMleBeta, beta) ## L2 error of the apesMleBeta vector with the true beta.
  )



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
####################################################################################
#######################
## If a vector of IC has a minimum, then returns a vector with that minimum labelled as symbol.
icOptimal = function(ic, symbol){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = rep("", length(ic))
    res[which.min(ic)] = symbol
  }
  return(res)
}
####################################
## Calculate the log-likelihood of logisitic regression using yPois and estimated pis
loglikeMu = function(yPois, mus){
  # loglike = yPois*log(pis) + (1-yPois)*log(1-pis)
  loglike = -mus + yPois * log(mus) - log(factorial(yPois))
  return(sum(loglike))
}
#####################################
## Calculate estimated pis using beta
beta2Mu = function(X, beta){
  xint = cbind(Int = 1,X)
  exp(xint[, names(beta)] %*% as.matrix(beta))
}
#####################################
## Given an indicators of variables, design matrix and the yPois, we refit the MLE-logisitic. X should not have an Intercept term
refittingMle_poisson = function(indicator, X, yPois){
  xTemp = cbind(Int = 1, X[,indicator])
  colnames(xTemp) = c("Int", colnames(X)[indicator])

  glm.fit(x = xTemp,
          y = yPois,
          # etastart = fullModel$linear.predictors,
          family = poisson(link = "log"))
}
