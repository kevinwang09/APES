#' Visualise model averaging results
#' @title Average of model average plot
#' @description This function is suitable for a list of bootstrap APES outputs.
#' From each bootstrap run, APES stores coefficient values averaged across all models considered.
#' As we have multiple bootstrapped APES output, we can cummulatively average
#' these model averaged coefficient values across all bootstrap runs.
#' On the final plot, we should be able to see variables of non-zero coefficients show up distinctly away from zero.
#' @param list_result a list of APES outputs
#' @param order either "BIC" (default) or "AIC"
#' @author Kevin Wang
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import directlabels
#' @importFrom magrittr %>%
#' @rdname plot.boot_apes
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' k = 1:10
#' beta = c(1, -1, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "binomial")
#'
#' list_result = apes(model = model, n_boot = 20)
#'
#' plot_ma_boot_apes(list_result = list_result)

plot_ma_boot_apes = function(list_result, order = "BIC"){
  model_avg_beta = purrr::map(list_result, "model_avg_beta")

  stopifnot(order %in% c("AIC", "BIC"))

  if(order == "AIC") {
    model_avg_beta_aic = purrr::map(model_avg_beta, ~.x[,"aic_weight_coef"]) %>%
      do.call(cbind, .)
    cummean_model_avg = apply(model_avg_beta_aic, 1, dplyr::cummean)
  }

  if(order == "BIC"){
    model_avg_beta_bic = purrr::map(model_avg_beta, ~.x[,"bic_weight_coef"]) %>%
      do.call(cbind, .)
    cummean_model_avg = apply(model_avg_beta_bic, 1, dplyr::cummean)
  }

  cummean_model_avg_plotdf = reshape2::melt(
    cummean_model_avg,
    varnames = c("cum_boot_num", "variables"),
    value.name = "ma_values")

  result = cummean_model_avg_plotdf %>%
    ggplot2::ggplot(aes(
      x = .data$cum_boot_num,
      y = .data$ma_values,
      colour = .data$variables,
      group = .data$variables,
      label = .data$variables)) +
    ggplot2::geom_line() +
    directlabels::geom_dl(
      method = list("last.qp",
                    cex = 1.2,
                    directlabels::dl.trans(x = x + 0.5))) +
    ggplot2::xlim(1, nrow(cummean_model_avg) + 5) +
    ggplot2::labs(
      title = paste("Cumulative MA coefficients using ", order),
      x = "Number of bootstraps",
      y = "Cumulative averaged MA coefficients") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "none")

  return(result)
}
