#' Variable inclusion plot in tile format
#' @title Variable Inclusion Plot in tile format
#' @param listResult a list of APES outputs
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
#' viTileResult = viTilePlot_apes(listResult)
#' viTileResult$variableTilePlot
#' viTileResult$variableTilePlot_category

viTilePlot_apes = function(listResult){
  apesMleBetaBinaryList = purrr::map_dfr(listResult, "apesMleBetaBinary", .id = "bootNum")
  apesModelDfList = purrr::map_dfr(listResult, "apesModelDf", .id = "bootNum")

  aicOptimalMedianSize = apesModelDfList %>% dplyr::filter(stringr::str_detect(icOptimalModels, "apesMinAic")) %>%
    dplyr::pull(modelSize) %>% median(na.rm = TRUE)
  bicOptimalMedianSize = apesModelDfList %>% dplyr::filter(stringr::str_detect(icOptimalModels, "apesMinBic")) %>%
    dplyr::pull(modelSize) %>% median(na.rm = TRUE)

  apesMleBetaBinaryPlotdf = apesMleBetaBinaryList %>%
    dplyr::group_by(variables, modelName) %>%
    dplyr::summarise(freqSelected = mean(fittedBeta)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      modelSize = stringr::str_replace_all(modelName, "apesModel_", "") %>% as.integer,
      variables = forcats::fct_reorder(
        variables, freqSelected, quantile, 0.5) %>%
        forcats::fct_relevel("Int") %>%
        forcats::fct_shift(),
      freqSelected_category = base::cut(
        freqSelected, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE
      )
    ) %>% as.tibble

  variableTilePlot = apesMleBetaBinaryPlotdf %>%
    ggplot2::ggplot(aes(x = modelSize,
               y = variables,
               fill = freqSelected)) +
    ggplot2::geom_tile(colour = "gray") +
    ggplot2::scale_x_continuous(breaks = seq(min(apesMleBetaBinaryPlotdf$modelSize),
                                             max(apesMleBetaBinaryPlotdf$modelSize), by = 1L)) +
    ggplot2::annotate("text", x = aicOptimalMedianSize + 0.2, y = "Int", label = "AIC", angle = 90) +
    ggplot2::annotate("text", x = bicOptimalMedianSize + 0.2, y = "Int", label = "BIC", angle = 90) +
    ggplot2::geom_vline(xintercept = aicOptimalMedianSize, colour = "black") +
    ggplot2::geom_vline(xintercept = bicOptimalMedianSize, colour = "black") +
    ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1) +
    ggplot2::theme(legend.text = element_text(angle = 45, hjust = 0.7),
                   legend.position = "bottom")

  # variableTilePlot

  variableTilePlot_category = apesMleBetaBinaryPlotdf %>%
    ggplot2::ggplot(aes(x = modelSize,
               y = variables,
               fill = freqSelected_category)) +
    ggplot2::geom_tile(colour = "gray") +
    ggplot2::annotate("text", x = aicOptimalMedianSize + 0.2, y = "Int", label = "AIC", angle = 90) +
    ggplot2::annotate("text", x = bicOptimalMedianSize + 0.2, y = "Int", label = "BIC", angle = 90) +
    ggplot2::geom_vline(xintercept = aicOptimalMedianSize, colour = "black") +
    ggplot2::geom_vline(xintercept = bicOptimalMedianSize, colour = "black") +
    ggplot2::scale_fill_manual(
      values = colorRampPalette(RColorBrewer::brewer.pal(3, "YlGnBu"))(5)
    ) +
    ggplot2::theme(legend.position = "bottom")

  # variableTilePlot_category

  result = list(
    apesMleBetaBinaryPlotdf = apesMleBetaBinaryPlotdf,
    variableTilePlot = variableTilePlot,
    variableTilePlot_category = variableTilePlot_category
  )


  return(result)
}
