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
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "binomial")
#'
#' list_result = apes(model = model, n_boot = 20)
#'
#' plot_vi_tile_boot_apes(list_result = list_result)
plot_vi_tile_boot_apes = function(list_result, order = "median"){
  apes_mle_beta_binary_bind = purrr::map_dfr(list_result, "apes_mle_beta_binary", .id = "boot_num")
  apes_model_df_bind = purrr::map_dfr(list_result, "apes_model_df", .id = "boot_num")

  aic_opt_median_size = apes_model_df_bind %>%
    dplyr::filter(stringr::str_detect(ic_opt_models, "apes_min_aic")) %>%
    dplyr::pull(model_size) %>% median(na.rm = TRUE)
  bic_opt_median_size = apes_model_df_bind %>%
    dplyr::filter(stringr::str_detect(ic_opt_models, "apes_min_bic")) %>%
    dplyr::pull(model_size) %>% median(na.rm = TRUE)

  apes_mle_beta_binary_plotdf = apes_mle_beta_binary_bind %>%
    dplyr::group_by(variables, model_name) %>%
    dplyr::summarise(freq_selected = mean(fitted_beta), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      model_size = stringr::str_replace_all(model_name, "apes_model_", "") %>% as.integer,
      freq_selected_category = base::cut(freq_selected, breaks = seq(0, 1, by = 0.2), include.lowest = TRUE)) %>%
    tibble::as_tibble()

  all_model_size = apes_mle_beta_binary_plotdf$model_size %>% unique

  if(order == "median"){
    apes_mle_beta_binary_plotdf = apes_mle_beta_binary_plotdf %>%
      dplyr::mutate(
        variables = forcats::fct_reorder(
          variables, freq_selected, stats::quantile, 0.5) %>%
          forcats::fct_relevel("intercept") %>%
          forcats::fct_shift())
  }

  if(order == "AIC"){
    by_aic = apes_mle_beta_binary_plotdf %>%
      dplyr::filter(model_size == all_model_size[which.min(abs(all_model_size - aic_opt_median_size))]) %>%
      dplyr::mutate(
        freq_selected = dplyr::coalesce(freq_selected, 0),
        variables = forcats::fct_reorder(
          variables, freq_selected) %>%
          forcats::fct_relevel("intercept") %>%
          forcats::fct_shift())

    apes_mle_beta_binary_plotdf = apes_mle_beta_binary_plotdf %>%
      dplyr::mutate(
        variables = forcats::fct_relevel(variables, levels(by_aic$variables)) %>%
          forcats::fct_relevel("intercept") %>%
          forcats::fct_shift())
  }


  if(order == "BIC"){
    by_bic = apes_mle_beta_binary_plotdf %>%
      dplyr::filter(model_size == all_model_size[which.min(abs(all_model_size - bic_opt_median_size))]) %>%
      dplyr::mutate(
        freq_selected = dplyr::coalesce(freq_selected, 0),
        variables = forcats::fct_reorder(
          variables, freq_selected) %>%
          forcats::fct_relevel("intercept") %>%
          forcats::fct_shift())

    apes_mle_beta_binary_plotdf = apes_mle_beta_binary_plotdf %>%
      dplyr::mutate(
        variables = forcats::fct_relevel(variables, levels(by_bic$variables)) %>%
          forcats::fct_relevel("intercept") %>%
          forcats::fct_shift())
  }



  var_tile_plot = apes_mle_beta_binary_plotdf %>%
    ggplot2::ggplot(aes(x = model_size,
               y = variables,
               fill = freq_selected)) +
    ggplot2::geom_tile(colour = "gray") +
    ggplot2::scale_x_continuous(breaks = seq(min(apes_mle_beta_binary_plotdf$model_size),
                                             max(apes_mle_beta_binary_plotdf$model_size), by = 1L)) +
    ggplot2::annotate("text", x = aic_opt_median_size + 0.2, y = "intercept", label = "AIC", angle = 90) +
    ggplot2::annotate("text", x = bic_opt_median_size + 0.2, y = "intercept", label = "BIC", angle = 90) +
    ggplot2::geom_vline(xintercept = aic_opt_median_size, colour = "black") +
    ggplot2::geom_vline(xintercept = bic_opt_median_size, colour = "black") +
    ggplot2::scale_fill_distiller(palette = "Spectral", direction = -1, breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1)) +
    ggplot2::labs(
      x = "Model size (including intercept)",
      y = "Variables",
      fill = "Selection frequency",
      title = "Variable inclusion tile plot, continuous colouring") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.text = element_text(angle = 90, vjust = 0.5),
                   legend.position = "bottom")


  var_tile_plot_category = apes_mle_beta_binary_plotdf %>%
    ggplot2::ggplot(aes(x = model_size,
               y = variables,
               fill = freq_selected_category)) +
    ggplot2::geom_tile(colour = "gray") +
    ggplot2::annotate("text", x = aic_opt_median_size + 0.2, y = "intercept", label = "AIC", angle = 90) +
    ggplot2::annotate("text", x = bic_opt_median_size + 0.2, y = "intercept", label = "BIC", angle = 90) +
    ggplot2::geom_vline(xintercept = aic_opt_median_size, colour = "black") +
    ggplot2::geom_vline(xintercept = bic_opt_median_size, colour = "black") +
    ggplot2::scale_fill_manual(
      values = colorRampPalette(RColorBrewer::brewer.pal(3, "YlGnBu"))(5)
    ) +
    ggplot2::labs(
      x = "Model size (including intercept)",
      y = "Variables",
      fill = "Selection frequency",
      title = "Variable inclusion tile plot") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "bottom")

  # variableTilePlot_category

  result = list(
    apes_mle_beta_binary_plotdf = apes_mle_beta_binary_plotdf,
    var_tile_plot = var_tile_plot,
    var_tile_plot_category = var_tile_plot_category
  )


  return(result)
}
