#' @title Model averaged coefficient plot for bootstrapped APES result.
#' @param x An object of class \code{boot_apes}
#' @param order either "BIC" (default) or "AIC"
#' @author Kevin Wang
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import directlabels
#' @importFrom magrittr %>%
#' @return A ggplot. From each bootstrap run, APES stores coefficient values averaged across all models considered.
#' As we have multiple bootstrapped APES output, we can cummulatively average
#' these model averaged coefficient values across all bootstrap runs.
#' On the final plot, we should be able to see variables of non-zero coefficients show up distinctly away from zero.
#' @rdname plot.boot_apes
#' @export
plot_ma_boot_apes = function(x, order = "BIC"){
  model_avg_beta = purrr::map(x, "model_avg_beta")

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
