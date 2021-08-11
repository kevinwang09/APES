#' @title Model averaged coefficient plot for bootstrapped APES result.
#' @param x An object of class \code{boot_apes}
#' @param order either "BIC" (default) or "AIC"
#' @author Kevin Wang
#' @importFrom dplyr %>%
#' @import ggplot2
#' @importFrom purrr map
#' @importFrom ggrepel geom_text_repel
#' @return A ggplot. From each bootstrap run, APES stores coefficient values averaged across all models considered.
#' As we have multiple bootstrapped APES output, we can cummulatively average
#' these model averaged coefficient values across all bootstrap runs.
#' On the final plot, we should be able to see variables of non-zero coefficients show up distinctly away from zero.
#' @rdname plot.boot_apes
#' @export
plot_boot_apes_ma = function(x, order = "BIC", max_vars = NULL){
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

  p = length(unique(cummean_model_avg_plotdf$variables))
  if(is.null(max_vars)){
    max_vars = p
  }

  label_tbl = cummean_model_avg_plotdf %>%
    dplyr::filter(.data$cum_boot_num == max(.data$cum_boot_num)) %>%
    dplyr::mutate(label = .data$variables %>%
                    forcats::fct_reorder(abs(.data$ma_values), .desc = TRUE),
                  label = ifelse(.data$label %in% levels(.data$label)[seq_len(max_vars)], as.character(.data$label), NA))

  result = cummean_model_avg_plotdf %>%
    ggplot2::ggplot(aes(
      x = .data$cum_boot_num,
      y = .data$ma_values,
      colour = .data$variables,
      group = .data$variables,
      label = .data$variables)) +
    ggplot2::geom_line() +
    ggrepel::geom_text_repel(data = label_tbl,
                             mapping = aes(label = .data$label),
                             direction = "y", seed = 1, segment.color = NA) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::xlim(1, nrow(cummean_model_avg) + 5) +
    ggplot2::labs(
      title = paste("Cumulative MA coefficients using ", order),
      x = "Number of bootstraps",
      y = "Cumulative averaged MA coefficients") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "none")

  return(result)
}
