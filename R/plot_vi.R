#' Variable inclusion plot
#' @title Variable Inclusion Plot
#' @param listResult a list of APES outputs
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
#' listResult = boot_apes_poisson(x = x, y = y, mu = mu, k = k, estimator = "leaps", nBoot = 50)
#' viPlotResult = plot_vi(listResult)
#' viPlotResult$viPlot



plot_vi = function(listResult){
  n = nrow(listResult[[1]]$responseTibble) ## Number of observations
  penalty = seq(0, 2*log(n), by = 0.1)
  list_apesModelDf = purrr::map(listResult, "apesModelDf")
  list_apesMleBetaBinary = purrr::map(listResult, "apesMleBetaBinary")

  bootOptimumVariablesDf = purrr::map2_dfr(
    .x = list_apesModelDf,
    .y = list_apesMleBetaBinary,

    ~ getOptimumVariables(
      penalty = penalty,
      apesModelDf = .x,
      apesMleBetaBinary = .y),
    .id = "bootNum")


  bootVarPlotdf = bootOptimumVariablesDf %>%
    group_by(penalty, variables) %>%
    summarise(bootSelectProb = mean(fittedBeta)) %>%
    ungroup() %>%
    dplyr::mutate(variables = as.character(variables))



  viPlot = bootVarPlotdf %>%
    ggplot2::ggplot(aes(x = penalty,
               y = bootSelectProb,
               colour = variables,
               group = variables,
               label = variables)) +
    directlabels::geom_dl(
      method = list("last.qp",
                    cex = 1.2,
                    directlabels::dl.trans(x = x + 0.2))) +
    ggplot2::geom_step(alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 2, colour = "black") +
    ggplot2::geom_vline(xintercept = log(n), colour = "black") +
    ggplot2::annotate("text", x = 2+0.2, y = 1, label = "AIC", angle = 90) +
    ggplot2::annotate("text", x = log(n)+0.2, y = 1, label = "BIC", angle = 90) +
    ggplot2::ylim(0,1) +
    ggplot2::xlim(min(penalty), max(penalty)+1) +
    ggplot2::theme(legend.position = "none")



  result = list(
    bootVarPlotdf = bootVarPlotdf,
    viPlot = viPlot
  )


  return(result)
}




getOptimumVariables = function(
  penalty,
  apesModelDf,
  apesMleBetaBinary){

  ## Get likeihood and model size to construct GIC
  logLike = apesModelDf$apesMleLoglike
  modelSize = apesModelDf$modelSize

  ## Construct the grip of values
  penaltyGrid = purrr::map(penalty,
                           ~ -2*logLike + .x * modelSize) %>%
    do.call(cbind, .)

  ## Find the minimum using GIC, for each penalty
  optimumIndex = penaltyGrid %>% apply(2, which.min)

  ## Find the models selected at each penalty
  optimumModelDf = dplyr::mutate(
    apesModelDf[optimumIndex,] %>%
      dplyr::select(
        modelName,
        modelSize,
        apesMleLoglike),
    penalty)

  ## Find the variables corresponding to each selected model under GIC
  optimumBetaBinary = purrr::map(
    optimumModelDf$modelName,
    ~ dplyr::filter(apesMleBetaBinary, modelName == .x)
  )
  names(optimumBetaBinary) = round(penalty, 5)

  ## Output the optimum variable data frame
  optimumVariableDataFrame = optimumBetaBinary %>%
    dplyr::bind_rows(.id = "penalty") %>%
    dplyr::mutate(penalty = as.numeric(penalty))

  return(optimumVariableDataFrame)
}
