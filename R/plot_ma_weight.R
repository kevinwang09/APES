#' @title MA weights visualisation
#' @description This function is suitable for a list of bootstrap APES outputs.
#' From each bootstrap run, APES stores AIC and BIC for every model it considered.
#' These AIC and BIC values can be converted into weights via AIC_j/sum(AIC_j), where AIC_j denotes the
#' AIC of the best model with j number of variables.
#' These weights can be used for calculate model averaging.
#' If a model size typically receives more weights across all bootstrap runs, then we could use this information
#' to determine the optimal number of variables to build into the final model.
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
#' plot_ma_weight(listResult, type = "AIC")
#' plot_ma_weight(listResult, type = "BIC")



plot_ma_weight = function(listResult, type = "AIC"){

  stopifnot(type %in% c("AIC", "BIC"))


  if(type == "AIC") {
    weightsMat = purrr::map(listResult,"aicWeights") %>%
      do.call(cbind, .)
  }

  if(type == "BIC"){
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
    ggplot2::labs(
      title = paste("Model average weights using", type),
      x = "Model size",
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
