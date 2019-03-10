#' @title AIC or BIC path plot
#' @param listResult a list of APES outputs
#' @param type Either "AIC" or "BIC"
#' @author Kevin Wang
#' @import ggplot2
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
#' listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 20)
#' bicPlot(listResult)




bicPlot = function(listResult, type = "BIC"){
  apesModelDf = purrr::map_dfr(listResult, "apesModelDf", .id = "bootNum") %>%
    group_by(bootNum) %>%
    dplyr::mutate(
      mleBIC_min = (mleBIC == min(mleBIC)),
      mleAIC_min = (mleAIC == min(mleAIC))
    )


  if(type == "BIC"){
    g = apesModelDf %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = modelSize, y = mleBIC, group = bootNum)) +
      ggplot2::geom_point(aes(x = modelSize, y = mleBIC, colour = mleBIC_min, size = mleBIC_min)) +
      ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
      ggplot2::scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0)) +
      ggplot2::theme_classic(18) +
      ggplot2::theme(legend.position = "bottom")
  }

  if(type == "AIC"){
    g = apesModelDf %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = modelSize, y = mleAIC, group = bootNum)) +
      ggplot2::geom_point(aes(x = modelSize, y = mleAIC, colour = mleAIC_min, size = mleAIC_min)) +
      ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
      ggplot2::scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0)) +
      ggplot2::theme_classic(18) +
      ggplot2::theme(legend.position = "bottom")
  }

  return(g)

}
