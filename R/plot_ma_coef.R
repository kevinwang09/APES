#' Visualise model averaging results
#' @title Average of model average plot
#' @param listResult a list of APES outputs
#' @param type either "aic" (default) or "bic"
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
#' plot_ma_coef(listResult, type = "aic")
#' plot_ma_coef(listResult, type = "bic")

plot_ma_coef = function(listResult, type = "aic"){
  modelAvgBeta = purrr::map(listResult,"modelAvgBeta")

  stopifnot(type %in% c("aic", "bic"))

  if(type == "aic") {
    modelAvgBetaAic = purrr::map(modelAvgBeta, ~.x[,"aicWeightCoef"]) %>%
      do.call(cbind, .)
    cummeanModelAvg = apply(modelAvgBetaAic, 1, dplyr::cummean)
  }

  if(type == "bic"){
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
    labs(x = "Number of bootstraps",
         y = "Cumulative averaged MA coefficients") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "none")

  return(result)

}
