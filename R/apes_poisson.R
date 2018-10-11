#' Perform APES-Poisson regression using either the leaps or bestsubset package
#' @param x The n by p design matrix for Poisson regression
#' @param y The response vector for Poisson regression of length n
#' @param mu Esimated means of each observation using a baseline model. It should be a vector of length n.
#' @param k Model size to explore. If leaps was selected as the estimator, then model up to size max(k) will be explored. If mio was selected as the estimator, then model sizes specified in k will be explored.
#' @param estimator Either "leaps" or "mio", which correspond to optimisation algorithms available in the leaps and bestsubset package, respectively.
#' @param time.limit The time limit for the maximum time allocated to each model size model when the "mio" estimator was selected. It will not affect the speed if leaps
#' @import leaps
#' @import tibble
#' @import magrittr
#' @import dplyr
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
#' apesLeapsResult = apes_poisson(x = x, y = y, mu = mu, k = k,
#'                                estimator = "leaps")
#'
#' apesMioResult = apes_poisson(x = x, y = y, mu = mu, k = k,
#'                                estimator = "mio", time.limit = 5)
#'
#' all.equal(apesLeapsResult$apesMleBeta, apesMioResult$apesMleBeta)
#'
#'
apes_poisson = function(x, y, mu, k, estimator = "leaps", time.limit = 60){
  variables = c("Int", colnames(x))
  yPois = y
  ###### Begin setting up linear regression #######
  n = nrow(x)
  p = ncol(x)
  linearY = log(mu) + (1/mu)*(yPois - mu)
  ###### End setting up linear regression #######

  if(estimator == "leaps"){
    ############# Begin leaps solver ##############
    apesT1 = Sys.time()

    apesRes = leaps::regsubsets(x = x, y = linearY,
                                really.big = T,
                                nbest = 1,
                                nvmax = max(k),
                                method = "exhaustive")
    apesT2 = Sys.time()
    print("Finished solving linear regression approximation")
    ############# End leaps solver ##############
    ## The computational time
    apesTimeDiff = difftime(apesT2, apesT1, units = "mins")
    ############# Begin tidying ##############
    ## Summary of the leaps object
    summaryApesRes = summary(apesRes)
    summaryApesResWhich = summaryApesRes$which
    ## Remove the intercept term
    apesIndicator = t(summaryApesResWhich[, -which(colnames(summaryApesResWhich) == "(Intercept)")])
    rownames(apesIndicator) = variables[-1]
    ## We always include intercept term!!
    apesModelSize = colSums(apesIndicator) + 1L
    ############# End tidying ##############
    status = "leaps_optimal"
  } ## End estimator == "leaps"


  if(estimator == "mio"){
    ############# Begin MIO solver ##############
    apesT1 = Sys.time()
    apesRes = bestsubset::bs(x = x,
                             y = linearY,
                             intercept = T,
                             k = k,
                             verbose = TRUE,
                             tol = 1e-6,
                             form = 1,
                             nruns = 50,
                             time.limit = time.limit)
    apesT2 = Sys.time()
    print("Finished solving linear regression approximation")
    ############# End MIO solver ##############
    ## The computational time
    apesTimeDiff = difftime(apesT2, apesT1, units = "mins")
    ############# Begin tidying ##############
    apesHosmerBeta = apesRes$beta
    apesIndicator = (apesHosmerBeta != 0)
    apesModelSize = colSums(apesIndicator) + 1L ## We always include intercept term!!
    ############# End tidying ##############
    status = apesRes$status
  }

  apesModelName = paste0("apesModel_", apesModelSize)

  ## We compute a model for each model size, based on apesIndicator
  apesMleModels = apply(apesIndicator, 2, function(indicator){
    refittingMle_poisson(indicator = indicator, X = x, yPois = yPois)
  })

  ## Compute and collect the beta coefficients
  apesMleBeta = mleModelToBeta(mleModels = apesMleModels, variables = variables)
  colnames(apesMleBeta) = apesModelName

  ## Compute model fit statistics
  apesMleMu = apply(apesMleBeta, 2, function(beta){beta2Mu(X = x, beta = beta)})
  apesMleLoglike = apply(apesMleMu, 2, function(mleMu){loglikeMu(yPois = yPois, mus = mleMu)})
  apesMleBIC = -2*apesMleLoglike + log(n)*apesModelSize
  apesMleAIC = -2*apesMleLoglike + 2*apesModelSize

  ############# End Exact Poisson solver ##############

  ############# Constructing model information ##################
  apesModelDf = tibble::tibble(
    method = "apes",
    modelName = apesModelName,
    modelSize = apesModelSize,
    apesMleLoglike = apesMleLoglike,
    mleAIC = apesMleAIC,
    mleBIC = apesMleBIC,
    status = status,
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

  ####################### Model averaging ######################
  scaleApesMleAIC = apesMleAIC - min(apesMleAIC)
  aicWeights = matrix(exp(-0.5*scaleApesMleAIC)/sum(exp(-0.5*scaleApesMleAIC)),
                      ncol = 1, byrow = TRUE)

  aicWeightCoef = apesMleBeta %*% aicWeights

  scaleApesMleBIC = apesMleBIC - min(apesMleBIC)
  bicWeights = matrix(exp(-0.5*scaleApesMleBIC)/sum(exp(-0.5*scaleApesMleBIC)),
                      ncol = 1, byrow = TRUE)
  bicWeightCoef = apesMleBeta %*% bicWeights

  modelAvgBeta = cbind(aicWeightCoef,
                       bicWeightCoef)

  colnames(modelAvgBeta) = c("aicWeightCoef", "bicWeightCoef")


  result = list(
    apesModelDf = apesModelDf,
    apesMleBeta = apesMleBeta,
    apesMleBetaBinary = apesMleBetaBinary,
    apesTimeDiff = apesTimeDiff,

    selectedModelBeta = selectedModelBeta,
    aicWeights = aicWeights,
    bicWeights = bicWeights,
    modelAvgBeta = modelAvgBeta,
    responseTibble = responseTibble
  )
  return(result)
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
