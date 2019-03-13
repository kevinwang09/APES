#' Visualise model averaging results
#' @title Average of model average plot
#' @description This function is suitable for a list of bootstrap APES outputs.
#' From each bootstrap run, APES stores coefficient values averaged across all models considered.
#' As we have multiple bootstrapped APES output, we can cummulatively average
#' these model averaged coefficient values across all bootstrap runs.
#' On the final plot, we should be able to see variables of non-zero coefficients show up distinctly away from zero.
#' @param listResult a list of APES outputs
#' @param type either "AIC" (default) or "BIC"
#' @author Kevin Wang
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import directlabels
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
#' listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 20)
#'
#' plot_ma_coef(listResult, type = "AIC")
#' plot_ma_coef(listResult, type = "BIC")

plot_ma_coef = function(listResult, type = "AIC"){
  modelAvgBeta = purrr::map(listResult,"modelAvgBeta")

  stopifnot(type %in% c("AIC", "BIC"))

  if(type == "AIC") {
    modelAvgBetaAic = purrr::map(modelAvgBeta, ~.x[,"aicWeightCoef"]) %>%
      do.call(cbind, .)
    cummeanModelAvg = apply(modelAvgBetaAic, 1, dplyr::cummean)
  }

  if(type == "BIC"){
    modelAvgBetaBic = purrr::map(modelAvgBeta, ~.x[,"bicWeightCoef"]) %>%
      do.call(cbind, .)
    cummeanModelAvg = apply(modelAvgBetaBic, 1, dplyr::cummean)
  }

  # modelAvgBetaAic %>% t %>%  matplot
  #
  # apply(modelAvgBetaAic, 1, cummean) %>% matplot()




  cummeanModelAvgPlotdf = reshape2::melt(
    cummeanModelAvg,
    varnames = c("cumBootNum", "variables"),
    value.name = "maValues")

  result = cummeanModelAvgPlotdf %>%
    ggplot2::ggplot(aes(
      x = cumBootNum,
      y = maValues,
      colour = variables,
      group = variables,
      label = variables)) +
    ggplot2::geom_line() +
    directlabels::geom_dl(
      method = list("last.qp",
                    cex = 1.2,
                    directlabels::dl.trans(x = x + 0.5))) +
    ggplot2::xlim(1, nrow(cummeanModelAvg) + 5) +
    ggplot2::labs(
      title = paste("Cumulative MA coefficients using ", type),
      x = "Number of bootstraps",
      y = "Cumulative averaged MA coefficients") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "none")
  return(result)

}
