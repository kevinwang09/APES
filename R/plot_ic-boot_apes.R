#' @title AIC or BIC path plot
#' @param list_result a list of APES outputs
#' @param order Either "BIC" (default) or "AIC"
#' @author Kevin Wang
#' @import ggplot2
#' @import purrr
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
#' plot_ic_boot_apes(list_result = list_result)

plot_ic_boot_apes = function(list_result, order = "BIC"){
  apes_model_df_bind = purrr::map_dfr(list_result, "apes_model_df", .id = "boot_num") %>%
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
                    y = "BIC for each bootstrap run") +
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
                    y = "AIC for each bootstrap run") +
      ggplot2::theme(legend.position = "bottom")
  }

  return(g)

}
