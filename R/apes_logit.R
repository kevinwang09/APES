#' Perform APES-logistic regression using either the leaps or bestsubset package
#' @param x The n by p design matrix for logistic regression
#' @param y The response vector for logistic regression of length n
#' @param Pi Esimated probabilities of each observation using a baseline model. It should be a vector of length n.
#' @param k Model size to explore. If leaps was selected as the estimator, then model up to size max(k) will be explored. If mio was selected as the estimator, then model sizes specified in k will be explored.
#' @param estimator Either "leaps" or "mio", which correspond to optimisation algorithms available in the leaps and bestsubset package, respectively.
#' @param time.limit The time limit for the maximum time allocated to each model size model when the "mio" estimator was selected. It will not affect the speed if leaps
#' @import leaps
#' @import tibble
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
#' @return A list of results:
#' \itemize{
#' \item{apesModelDf:} A data.frame (tibble) containing the optimal model of each size and associated statistics
#' \item{apesMleBeta:} A matrix of MLE for the most optimal model of each model size.
#' \item{apesMleBetaBinary:} A data.frame (tibble), which has a column indicating if
#' a variable has been selected into the most optimal model of each size.
#' It is a binarised, linear version of the previous \code{apesMleBeta} output.
#' \item{apesTimeDiff:} A difftime object of the computational time.
#' \item{selectedModelBeta:} A matrix containing the MLE correspond to the AIC and BIC selected model. A subset of the \code{apesMleBeta} output.
#' \item{aicWeights:} A matrix of weights corresponding to the AIC_j/sum(AIC_j), where AIC_j denotes the most optimal model of the size j in the \code{apesModelDf} output.
#' \item{bicWeights:} A matrix of weights corresponding to the BIC_j/sum(BIC_j), where BIC_j denotes the most optimal model of the size j in the \code{apesModelDf} output.
#' \item{modelAvgBeta}: A matrix of AIC and BIC weighted coefficients. It is identical to the matrix product of apesMleBeta and aicWeights (or bicWeights).
#' \item{responseTibble}: A data.frame (tibble) of estimates for each observations, e.g. initial fitted response, final fitted response.
#' }
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' k = 1:10
#' beta = c(1, -1, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' Pi = glm.fit(x = x, y = y, family = binomial(link = "logit"))$fitted.values
#' apesLeapsResult = apes_logit(x = x, y = y, Pi = Pi, k = k,
#'                                estimator = "leaps")
#' apesLeapsResult$apesModelDf
#'
#' \dontrun{
#' ## You need to install Gurobi before uing the mio option
#' apesMioResult = apes_logit(x = x, y = y, Pi = Pi, k = k,
#'                                estimator = "mio", time.limit = 5)
#' }
apes_logit = function(x, y, Pi, k, estimator = "leaps", time.limit = 60){

  ###### Begin setting up linear regression #######
  variables = c("Int", colnames(x))
  yBinom = y

  n = nrow(x)
  p = ncol(x)

  epsilon = 1e-10

  if(sum(Pi < epsilon | Pi > 1-epsilon) >= floor(n/2)){
    warning("Proceed with caution: \n
            Over half of Pi's has values less than 1e-10 or greater than
            1 - 1e-10, which may imply degeneracy.")
  }
  linearY = log(Pi/(1-Pi)) + (yBinom-Pi)/(Pi*(1-Pi))
  ###### End setting up linear regression #######



  if(estimator == "leaps"){
    apesT1 = Sys.time()
    apesRes = leaps::regsubsets(x = x, y = linearY,
                                really.big = T,
                                nbest = 1,
                                nvmax = max(k),
                                method = "exhaustive")
    apesT2 = Sys.time()
    print("Finished solving linear regression approximation")
    ## The computational time
    apesTimeDiff = difftime(apesT2, apesT1, units = "mins")
    ############# Begin tidying ##############
    ## Summary of the leaps object
    summaryApesRes = summary(apesRes)
    summaryApesResWhich = summaryApesRes$which
    ## Remove the intercept term
    apesIndicator = t(summaryApesResWhich[, -which(colnames(summaryApesResWhich) == "(Intercept)"), drop = FALSE])
    rownames(apesIndicator) = variables[-1]
    ## We always include intercept term!!
    apesModelSize = colSums(apesIndicator) + 1L
    ############# End tidying ##############
    status = "leaps_optimal"
  } ## End estimator == "leaps"


  if(estimator == "mio"){

    if (!requireNamespace("bestsubset", quietly = TRUE)) {
      stop("Package \"bestsubset\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

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
    # print("Finished solving linear regression approximation")
    ## The computational time
    apesTimeDiff = difftime(apesT2, apesT1, units = "mins")
    ############# Begin tidying ##############
    apesHosmerBeta = apesRes$beta
    apesIndicator = (apesHosmerBeta != 0)
    ## We always include intercept term!!
    apesModelSize = colSums(apesIndicator) + 1L
    ############# End tidying ##############
    status = apesRes$status
  }


  ############ Generating evaluation outputs ############
  apesModelName = paste0("apesModel_", apesModelSize)

  ## We compute a model for each model size, based on apesIndicator
  apesMleModels = apply(apesIndicator, 2, function(indicator){
    refittingMle_logit(indicator = indicator, X = x, yBinom = yBinom)
  })

  ## Compute and collect the beta coefficients
  apesMleBeta = mleModelToBeta(mleModels = apesMleModels, variables = variables)
  colnames(apesMleBeta) = apesModelName

  ## Compute model fit statistics
  apesMlePi = apply(apesMleBeta, 2, function(beta){beta2Pi(X = x, beta = beta)})
  apesMleLoglike = apply(apesMlePi, 2, function(mlePi){loglikePi(yBinom = yBinom, pis = mlePi)})
  apesMleBIC = -2*apesMleLoglike + log(n)*apesModelSize
  apesMleAIC = -2*apesMleLoglike + 2*apesModelSize

  ############# Constructing model data frame ##################
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
  )
  ############# Constructing estimated Probability information ##################

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
                                  y = yBinom,
                                  Pi = Pi,
                                  linearY = linearY,
                                  apesMinAicProb = apesMinAicProb,
                                  apesMinBicProb = apesMinBicProb) %>%
    tibble::as.tibble()

  apesMleBetaBinary = reshape2::melt(apesMleBeta != 0,
                                     varnames = c("variables", "modelName"),
                                     value.name = "fittedBeta") %>% tibble::as.tibble()

  selectedModelBeta = cbind(
    apesMinAic = minIcMatrix(ic = apesMleAIC, mat = apesMleBeta),
    apesMinBic = minIcMatrix(ic = apesMleBIC, mat = apesMleBeta)
  )

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

  # maxBicValue = max(apesMleBIC)
  # bicValueMaxRemoved = apesMleBIC[-which.max(apesMleBIC)]
  #
  # logA0 = -0.5*maxBicValue
  # logAi = -0.5*bicValueMaxRemoved
  #
  # logA0 + log(1 + sum(exp(logAi - logA0)))
  ######################## End Model averaging ################

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
####################################
####################################
####################################
####################################
#####################################
icOptimal = function(ic, symbol){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = rep("", length(ic))
    res[which.min(ic)] = symbol
  }
  return(res)
}
######################################
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
## Given an indicators of variables, design matrix and the yBinom, we refit the MLE-logisitic. X should not have an Intercept term
refittingMle_logit = function(indicator, X, yBinom){
  xTemp = cbind(Int = 1, X[,indicator])
  colnames(xTemp) = c("Int", colnames(X)[indicator])

  stats::glm.fit(x = xTemp,
                 y = yBinom,
                 # etastart = fullModel$linear.predictors,
                 family = stats::binomial(link = "logit"))
}
#####################################
icOptimal = function(ic, symbol){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = rep("", length(ic))
    res[which.min(ic)] = symbol
  }
  return(res)
}
#####################################
minIcMatrix = function(ic, mat){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = mat[,which.min(ic)]
  }
  return(res)
}
#####################################
mleModelToBeta = function(mleModels, variables){
  mleBeta = purrr::map(mleModels, "coefficients") %>%
    purrr::map(t) %>%
    purrr::map(data.frame) %>%
    dplyr::bind_rows() %>% t

  mleBeta[is.na(mleBeta)] = 0L
  variablesOrdered = base::intersect(rownames(mleBeta), variables) %>% gtools::mixedsort()
  mleBeta = mleBeta[variablesOrdered,,drop = FALSE]

  tmp = matrix(0L,
               nrow = length(variables),
               ncol = ncol(mleBeta),
               dimnames = list(variables, colnames(mleBeta))) ## To avoid a variable is never selected, we create an empty matrix

  tmp[rownames(mleBeta), ] = mleBeta

  return(tmp)
}
