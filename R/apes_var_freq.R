#' @title Calculates the frequency that a variable is selected
#' @param listResult a list of APES outputs
#' @param ic Either "AIC" or "BIC"
#' @author Kevin Wang
#' @import dplyr
#' @import stringr
#' @import purrr
#' @importFrom magrittr %>%
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
#'
#' listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 50)
#' apes_var_freq(listResult = listResult, ic = "AIC")
#' apes_var_freq(listResult = listResult, ic = "BIC")

apes_var_freq = function(listResult, ic = "BIC"){
  apesModelDf = purrr::map_dfr(listResult, "apesModelDf",  .id = "bootNum")
  apesMleBetaBinaryDf = purrr::map_dfr(listResult, "apesMleBetaBinary",  .id = "bootNum") %>%
    dplyr::mutate(bootNum_modelName = base::paste(bootNum, modelName, sep = "_"))

  if(ic == "AIC"){
    icOptimalModels = apesModelDf %>%
      dplyr::filter(stringr::str_detect(icOptimalModels, "apesMinAic")) %>%
      dplyr::mutate(bootNum_modelName = base::paste(bootNum, modelName, sep = "_"))

    cat("Summary of model sizes selected by AIC \n")
  }

  if(ic == "BIC"){
    icOptimalModels = apesModelDf %>%
      dplyr::filter(stringr::str_detect(icOptimalModels, "apesMinBic")) %>%
      dplyr::mutate(bootNum_modelName = base::paste(bootNum, modelName, sep = "_"))

    cat("Summary of model sizes selected by BIC \n")

  }

  icOptimalVariables = apesMleBetaBinaryDf %>%
    dplyr::filter(bootNum_modelName %in% icOptimalModels$bootNum_modelName)

  icOptimalVariables_freq = icOptimalVariables %>%
    dplyr::group_by(variables) %>%
    dplyr::summarise(freq = mean(fittedBeta)) %>%
    dplyr::arrange(dplyr::desc(freq)) %>%
    dplyr::ungroup()

  return(icOptimalVariables_freq)
}
