#' Variable inclusion plot in tile format
#' @title Variable Inclusion Plot in tile format
#' @description This function displays the same information as plot_vi, but in a tile plot format.
#' @param listResult a list of APES outputs
#' @param order The ordering of variables. Either "median", "AIC" or "BIC"
#' @author Kevin Wang
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import directlabels
#' @import RColorBrewer
#' @import forcats
#' @importFrom magrittr %>%
#' @return apesMleBetaBinaryPlotdf a tibble (data.frame) with all the necessary values to plot a variable inclusion plot
#' @return variableTilePlot a ggplot with continuous colouring
#' @return variableTilePlot_category a ggplot with discrete colouring
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
#' listResult = boot_apes_poisson(
#' x = x, y = y, mu = mu, k = k,
#' estimator = "leaps", nBoot = 20)
#' viTileResult = plot_vi_tile(listResult = listResult, order = "median")
#' viTileResult$variableTilePlot
#' viTileResult$variableTilePlot_category
#' viTileResult2 = plot_vi_tile(listResult = listResult, order = "AIC")
#' viTileResult2$variableTilePlot ## Note the different order on the AIC line

plot_vi_tile = function(listResult, order = "median"){
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
      freqSelected_category = base::cut(freqSelected, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)) %>%
    tibble::as_tibble()

  allModelSize = apesMleBetaBinaryPlotdf$modelSize %>% unique

  if(order == "median"){
    apesMleBetaBinaryPlotdf = apesMleBetaBinaryPlotdf %>%
      dplyr::mutate(
        variables = forcats::fct_reorder(
          variables, freqSelected, stats::quantile, 0.5) %>%
          forcats::fct_relevel("Int") %>%
          forcats::fct_shift())
  }

  if(order == "AIC"){
    byAIC = apesMleBetaBinaryPlotdf %>%
      dplyr::filter(modelSize == allModelSize[which.min(abs(allModelSize - aicOptimalMedianSize))]) %>%
      dplyr::mutate(
        freqSelected = dplyr::coalesce(freqSelected, 0),
        variables = forcats::fct_reorder(
          variables, freqSelected) %>%
          forcats::fct_relevel("Int") %>%
          forcats::fct_shift())

    apesMleBetaBinaryPlotdf = apesMleBetaBinaryPlotdf %>%
      dplyr::mutate(
        variables = forcats::fct_relevel(variables, levels(byAIC$variables)) %>%
          forcats::fct_relevel("Int") %>%
          forcats::fct_shift())
  }


  if(order == "BIC"){
    byBIC = apesMleBetaBinaryPlotdf %>%
      dplyr::filter(modelSize == allModelSize[which.min(abs(allModelSize - bicOptimalMedianSize))]) %>%
      dplyr::mutate(
        freqSelected = dplyr::coalesce(freqSelected, 0),
        variables = forcats::fct_reorder(
          variables, freqSelected) %>%
          forcats::fct_relevel("Int") %>%
          forcats::fct_shift())

    apesMleBetaBinaryPlotdf = apesMleBetaBinaryPlotdf %>%
      dplyr::mutate(
        variables = forcats::fct_relevel(variables, levels(byBIC$variables)) %>%
          forcats::fct_relevel("Int") %>%
          forcats::fct_shift())
  }



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
    ggplot2::labs(
      title = "Variable inclusion tile plot, continuous colouring") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.text = element_text(angle = 90, hjust = 0.7),
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
    ggplot2::labs(
      title = "Variable inclusion tile plot, discrete colouring") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "bottom")

  # variableTilePlot_category

  result = list(
    apesMleBetaBinaryPlotdf = apesMleBetaBinaryPlotdf,
    variableTilePlot = variableTilePlot,
    variableTilePlot_category = variableTilePlot_category
  )


  return(result)
}
