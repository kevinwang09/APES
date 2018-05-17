#' Perform APES using the leaps package
#' @param x n times p design matrix for logistics regression
#' @param y response vector for logistics regression of length n
#' @param Pi esimated probability of each observation using a baseline model. Of length n.
#' @param maxK maximum model size, less than p.
#' @import leaps
#' @import tibble
#' @import broom
#' @import magrittr
#' @import dplyr
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' maxK = 5
#
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = round(runif(n, 0,1))
#' Pi = runif(n, 0, 1)
#' apesResult = apes_leaps(x = x, y = y, Pi = Pi, maxK = maxK)
#' names(apesResult)

apes_leaps = function(x,
                      y,
                      Pi,
                      maxK){

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
    refittingMle(indicator = indicator, X = x, yBinom = yBinom)
  }) ## Each model will be different


  apesMleBeta = mleModelToBeta(mleModels = apesMleModels, variables = variables)
  colnames(apesMleBeta) = apesModelName

  apesMlePi = apply(apesMleBeta, 2, function(beta){beta2Pi(X = x, beta = beta)})
  apesMleLoglike = apply(apesMlePi, 2, function(mlePi){loglikePi(yBinom = yBinom, pis = mlePi)})
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
  responseTibble = tibble::tibble(obsNum = obsNum,
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
####################################################################################
expit = function(x){
  1/(1+exp(-x))
}

logit = function(x){
  log(x) - log(1-x)
}
##############################
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
##############################
minIcMatrix = function(ic, mat){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = mat[,which.min(ic)]
  }
  return(res)
}
####################################
## Calculate the log-likelihood of logisitic regression using yBinom and estimated pis
loglikePi = function(yBinom, pis){
  loglike = yBinom*log(pis) + (1-yBinom)*log(1-pis)
  return(sum(loglike))
}
#####################################
## Calculate estimated pis using beta
beta2Pi = function(X, beta){
  xint = cbind(Int = 1,X)
  expit(xint[, names(beta)] %*% as.matrix(beta))
}
#####################################
#####################################
## Given an indicators of variables, design matrix and the yBinom, we refit the MLE-logisitic. X should not have an Intercept term
refittingMle = function(indicator, X, yBinom){
  xTemp = cbind(Int = 1, X[,indicator])
  colnames(xTemp) = c("Int", colnames(X)[indicator])

  glm.fit(x = xTemp,
          y = yBinom,
          # etastart = fullModel$linear.predictors,
          family = binomial(link = "logit"))
}
#####################################
## We take the MLE model fitted above and rearrange the columns and remove the NA's in our vector
mleModelToBeta = function(mleModels, variables){
  mleBeta = purrr::map(mleModels, "coefficients") %>%
    purrr::map(t) %>%
    purrr::map(data.frame) %>%
    dplyr::bind_rows() %>% t

  mleBeta[is.na(mleBeta)] = 0L
  variablesOrdered = intersect(rownames(mleBeta), variables) %>% gtools::mixedsort()
  # variablesOrdered = variables %>% mixedsort
  mleBeta = mleBeta[variablesOrdered,]

  tmp = matrix(0L,
               nrow = length(variables),
               ncol = ncol(mleBeta),
               dimnames = list(variables, colnames(mleBeta))) ## To avoid a variable is never selected, we create an empty matrix

  tmp[rownames(mleBeta), ] = mleBeta

  return(tmp)
}
