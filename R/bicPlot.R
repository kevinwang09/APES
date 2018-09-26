#' Variable inclusion plot
#' @title Variable Inclusion Plot
#' @param listResult a list of APES outputs
#' @author Kevin Wang
#' @import ggplot2
#' @import purrr
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
#' aicPlot(listResult)




bicPlot = function(listResult){
  apesModelDf = purrr::map_dfr(listResult, "apesModelDf", .id = "bootNum") %>% 
    group_by(bootNum) %>% 
    dplyr::mutate(
      mleBIC_min = (mleBIC == min(mleBIC)),
      mleAIC_min = (mleAIC == min(mleAIC))
    )
  
  apesModelDf %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = modelSize, y = mleBIC, group = bootNum)) +
    ggplot2::geom_point(aes(x = modelSize, y = mleBIC, colour = mleBIC_min, size = mleBIC_min)) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
    scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0)) +
    theme_classic(18) + 
    theme(legend.position = "bottom")
  
}