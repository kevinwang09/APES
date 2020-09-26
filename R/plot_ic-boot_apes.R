#' @title AIC or BIC path plot for bootstrapped APES result
#' @param x An object of class \code{boot_apes}
#' @param order Either "BIC" (default) or "AIC"
#' @author Kevin Wang
#' @import ggplot2
#' @import purrr
#' @importFrom magrittr %>%
#' @rdname plot.boot_apes
#' @return A ggplot of AIC/BIC path plot. Each curve is one bootstrapped APES run.
#' @export
plot_path_boot_apes = function(x, order = "BIC"){
  apes_model_df_bind = purrr::map_dfr(x, "apes_model_df", .id = "boot_num") %>%
    group_by(boot_num) %>%
    dplyr::mutate(
      mle_bic_min = (mle_bic == min(mle_bic)),
      mle_aic_min = (mle_aic == min(mle_aic))
    )


  if(order == "BIC"){
    g = apes_model_df_bind %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = .data$model_size, y = .data$mle_bic, group = .data$boot_num)) +
      ggplot2::geom_point(aes(x = .data$model_size, y = .data$mle_bic, colour = .data$mle_bic_min, size = .data$mle_bic_min)) +
      ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
      ggplot2::scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0)) +
      ggplot2::theme_classic(18) +
      ggplot2::labs(title = "APES BIC path plot",
                    x = "Model size (including intercept)",
                    y = "BIC") +
      ggplot2::theme(legend.position = "bottom")
  }

  if(order == "AIC"){
    g = apes_model_df_bind %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = .data$model_size, y = .data$mle_aic, group = .data$boot_num)) +
      ggplot2::geom_point(aes(x = .data$model_size, y = .data$mle_aic, colour = .data$mle_aic_min, size = .data$mle_aic_min)) +
      ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
      ggplot2::scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0)) +
      ggplot2::theme_classic(18) +
      ggplot2::labs(title = "APES AIC path plot",
                    x = "Model size (including intercept)",
                    y = "AIC") +
      ggplot2::theme(legend.position = "bottom")
  }

  return(g)
}
