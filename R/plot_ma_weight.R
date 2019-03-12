#' MA weights visualisation
#' @title MA weights visualisation
#' @param listResult a list of APES outputs
#' @param type either "aic" (default) or "bic"
#' @author Kevin Wang
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import directlabels
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
#' plot_ma_weight(listResult, type = "aic")
#' plot_ma_weight(listResult, type = "bic")



plot_ma_weight = function(listResult, type = "aic"){

  stopifnot(type %in% c("aic", "bic"))


  if(type == "aic") {
    weightsMat = purrr::map(listResult,"aicWeights") %>%
      do.call(cbind, .)
  }

  if(type == "bic"){
    weightsMat = purrr::map(listResult,"bicWeights") %>%
      do.call(cbind, .)
  }

  colnames(weightsMat) = names(listResult)
  rownames(weightsMat) = listResult[[1]]$apesModelDf$modelSize

  weightsPlotdf = reshape2::melt(
    weightsMat,
    varnames = c("modelSize", "bootNum"),
    value.name = "maWeights")


  weightsPlotdf %>%
    ggplot2::ggplot(
      aes(x = factor(modelSize),
          y = maWeights)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = "Model size",
                  y = "MA weights") +
    ggplot2::theme_classic(18)



  # geom_point()

  # weightsPlotdf %>%
  #   ggplot(aes(x = modelSize,
  #              y = maWeights,
  #              group = bootNum)) +
  #   geom_point() +
  #   geom_smooth(aes(group = 1))
  # geom_line()

}
