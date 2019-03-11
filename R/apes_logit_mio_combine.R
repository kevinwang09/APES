#' Combine apes_logit results
#' @title Bootstrap version of apes_logit
#' @param listApes a list of APES output with the MIO optimisation algorithm
#' @author Kevin Wang
#' @import tibble
#' @import dplyr
#' @export
#' @examples
#'
#' ## You need to install Gurobi before uing the mio option
#' \dontrun{
#' set.seed(10)
#' n = 100
#' p = 10
#' k = 1:10
#' beta = c(1, -1, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' Pi = glm.fit(x = x, y = y, family = binomial(link = "logit"))$fitted.values
#' firstApes = apes_logit(x = x, y = y, Pi = Pi, k = 1, estimator = "mio")
#' secondApes = apes_logit(x = x, y = y, Pi = Pi, k = 2, estimator = "mio")
#' listApes = list(firstApes, secondApes)
#' finalResult = apes_logit_mio_combine(listApes)
#' print(finalResult)
#' }



apes_logit_mio_combine = function(listApes){
  if(is.null(names(listApes))){
    names(listApes) = seq_len(length(listApes))
  }
  apesModelDf = purrr::map(listApes, "apesModelDf")
  apesMleBeta = purrr::map(listApes, "apesMleBeta")
  apesMleBetaBinary = purrr::map(listApes, "apesMleBetaBinary")
  apesTimeDiff = purrr::map(listApes, "apesTimeDiff")
  selectedModelBeta = purrr::map(listApes, "selectedModelBeta")
  responseTibble = purrr::map(listApes, "responseTibble")




  apesModelDf_new = dplyr::bind_rows(apesModelDf, .id = "listName") %>%
    dplyr::mutate(icOptimalModels = paste0(
      icOptimal(ic = mleAIC, "apesMinAic"),
      icOptimal(ic = mleBIC, "apesMinBic")
    ))

  apesMleBeta_new = do.call(cbind, apesMleBeta)
  apesMleBetaBinary_new = dplyr::bind_rows(apesMleBetaBinary, .id = "listName")

  apesTimeDiff_new = do.call(c, apesTimeDiff)
  whichAicModel = which.min(apesModelDf_new$mleAIC)
  whichBicModel = which.min(apesModelDf_new$mleBIC)
  selectedModelBeta_new = cbind(selectedModelBeta[[whichAicModel]][,"apesMinAic"],
                                selectedModelBeta[[whichBicModel]][,"apesMinBic"])
  apesMinAicProb = purrr::map(responseTibble, "apesMinAicProb")
  apesMinBicProb = purrr::map(responseTibble, "apesMinBicProb")

  responseTibble_new =
    cbind(responseTibble[[1]][, c("obsNum", "y", "Pi", "linearY")],
          apesMinAicProb = apesMinAicProb[[whichAicModel]],
          apesMinBicProb = apesMinBicProb[[whichBicModel]]) %>% tibble::as.tibble()



  scaleApesMleAIC = apesModelDf_new$mleAIC - min(apesModelDf_new$mleAIC)
  aicWeights = matrix(exp(-0.5*scaleApesMleAIC)/sum(exp(-0.5*scaleApesMleAIC)),
                      ncol = 1, byrow = TRUE)

  aicWeightCoef = apesMleBeta_new %*% aicWeights

  scaleApesMleBIC = apesModelDf_new$mleBIC - min(apesModelDf_new$mleBIC)
  bicWeights = matrix(exp(-0.5*scaleApesMleBIC)/sum(exp(-0.5*scaleApesMleBIC)),
                      ncol = 1, byrow = TRUE)
  bicWeightCoef = apesMleBeta_new %*% bicWeights

  modelAvgBeta = cbind(aicWeightCoef,
                       bicWeightCoef)

  colnames(modelAvgBeta) = c("aicWeightCoef", "bicWeightCoef")

  result = list(
    apesModelDf = apesModelDf_new,
    apesMleBeta = apesMleBeta_new,
    apesMleBetaBinary = apesMleBetaBinary_new,
    apesTimeDiff = apesTimeDiff_new,
    selectedModelBeta = selectedModelBeta_new,
    responseTibble = responseTibble_new,
    modelAvgBeta = modelAvgBeta,
    aicWeights = aicWeights,
    bicWeights =  bicWeights
  )

  return(result)
}
