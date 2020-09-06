#' Perform APES Cox PH regression using either the leaps or bestsubset package
#' @param x The n by p design matrix for logistic regression
#' @param time survival time in survival analysis
#' @param status survival status in survival analysis
#' @param k Model size to explore. If "leaps" was selected as the estimator, then model up to size max(k) will be explored.
#' If "mio" was selected as the estimator, then model sizes specified in k will be explored.
#' @param estimator Either "leaps" or "mio", which correspond to optimisation algorithms available in the leaps and bestsubset package, respectively.
#' @param time.limit The time limit for the maximum time allocated to each model size model when the "mio" estimator was selected. It will not affect the speed if leaps
#' @import leaps
#' @import tibble
#' @importFrom gtools mixedsort
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom survival coxph
#' @importFrom survival Surv
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
#' set.seed(10101)
#' n = 200
#' p = 10
#' nzc = p/3
#' x = matrix(rnorm(n*p), n, p)
#' colnames(x) = paste0("X", 1:p)
#' beta = runif(round(nzc))
#' fx = x[,seq(nzc)] %*% beta
#' hx = exp(fx)
#' ty = rexp(n, hx)
#' tcens = rbinom(n = n, prob = 0.3, size = 1) # censoring indicator
#' time = ty
#' status = 1-tcens
#' apesLeapsResult = apes_cox(x = x, time = time, status = status, k = p,
#' estimator = "leaps")
#' \dontrun{
#' apesMioResult = apes_cox(x = x, time = time, status = status, k = 1:p,
#' estimator = "mio")
#' }
apes_cox = function(x, time, status, k, estimator = "leaps", time.limit = 60){

  ###### Begin setting up linear regression #######
  variables = c("Int", colnames(x))

  n = nrow(x)
  p = ncol(x)

  epsilon = 1e-10

  df = data.frame(x, time = time, status = status)
  full_cox = survival::coxph(survival::Surv(time, status) ~ ., data = df)

  linearY = full_cox$linear.predictors
  ###### End setting up linear regression #######

  if(estimator == "leaps"){
    apesT1 = Sys.time()
    apesRes = leaps::regsubsets(x = x, y = linearY,
                                really.big = TRUE,
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
    conv_status = "leaps_optimal"
  } ## End estimator == "leaps"


  if(estimator == "mio"){

    if (!requireNamespace("bestsubset", quietly = TRUE)) {
      stop("Package \"bestsubset\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    apesT1 = Sys.time()
    apesRes = bestsubset::bs(x = x,
                             y = linearY,
                             intercept = TRUE,
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
    conv_status = apesRes$status
  }


  ############ Generating evaluation outputs ############
  apesModelName = paste0("apesModel_", apesModelSize)

  ## We compute a model for each model size, based on apesIndicator
  apesMleModels = apply(apesIndicator, 2, function(indicator){
    refitting_cox(indicator = indicator, x = x, time = time, status = status)
  })

  ## Compute and collect the beta coefficients
  apesMleBeta = mleModelToBeta(mleModels = apesMleModels, variables = variables)
  colnames(apesMleBeta) = apesModelName

  ## Compute model fit statistics
  apesMleLoglike = purrr::map_dbl(apesMleModels, stats::logLik)
  apesMleBIC = purrr::map_dbl(apesMleModels, stats::BIC)
  apesMleAIC = purrr::map_dbl(apesMleModels, stats::AIC)

  ############# Constructing model data frame ##################
  apesModelDf = tibble::tibble(
    method = "apes",
    modelName = apesModelName,
    modelSize = apesModelSize,
    apesMleLoglike = apesMleLoglike,
    mleAIC = apesMleAIC,
    mleBIC = apesMleBIC,
    conv_status = conv_status,
    icOptimalModels = paste0(
      icOptimal(ic = apesMleAIC, "apesMinAic"),
      icOptimal(ic = apesMleBIC, "apesMinBic")
    ) ## This variable records where the min AIC and min BIC models are.
  )

  ############# Constructing estimated Probability information ##################

  obsNum = paste0("obs", 1:n)
  responseTibble = tibble::tibble(obsNum = obsNum,
                                  linearY = linearY) %>%
    tibble::as_tibble()

  apesMleBetaBinary = reshape2::melt(apesMleBeta != 0,
                                     varnames = c("variables", "modelName"),
                                     value.name = "fittedBeta") %>% tibble::as_tibble()

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
## Given an indicators of variables, design matrix and the yBinom, we refit the Cox. X should not have an Intercept term
refitting_cox = function(indicator, x, y){
  # df_tmp = data.frame(x[,indicator], time, status)
  # colnames(df_tmp) = c(colnames(x)[indicator], "time", "status")
  xtmp = x[,indicator, drop = FALSE]
  return(survival::coxph(y ~ xtmp))
}
#####################################
