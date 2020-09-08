#' @title Variable Inclusion Plot for bootstrapped APES result
#' @description This function is suitable for a list of bootstrap APES outputs.
#' From each bootstrap run, APES stores log-likelihood for every model it considered.
#' In this function, we then consider general information criterion (GIC) of the form
#' -2*logLike + penalty * modeSize.
#' For each penalty and each bootstrap run, we apply this GIC to find a model of the optimal fit, and then
#' look at which variables are selected in that model.
#' The frequency of a variable selected across different penalties are then avaraged across all bootstrap runs.
#' @param x An object of class \code{boot_apes}
#' @author Kevin Wang
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import directlabels
#' @importFrom magrittr %>%
#' @return A list. \itemize{
#' \item \code{boot_vars_plotdf} a tibble with all the necessary values to plot a variable inclusion plot
#' \item \cite{vip} a variable inclusion plot in ggplot format.
#' }
#' @rdname plot.boot_apes
#' @export
plot_vip_boot_apes = function(x){
  n = nrow(x[[1]]$response_tibble) ## Number of observations

  penalty = seq(0, 2*log(n), by = 0.1)
  list_apes_model_df = purrr::map(x, "apes_model_df")
  list_apes_mle_beta_binary = purrr::map(x, "apes_mle_beta_binary")

  boot_opt_vars = purrr::map2_dfr(
    .x = list_apes_model_df,
    .y = list_apes_mle_beta_binary,

    ~ get_opt_vars(
      penalty = penalty,
      apes_model_df = .x,
      apes_mle_beta_binary = .y),
    .id = "boot_num")


  boot_vars_plotdf = boot_opt_vars %>%
    group_by(penalty, variables) %>%
    summarise(boot_select_prob = mean(fitted_beta), .groups = "drop") %>%
    ungroup() %>%
    dplyr::mutate(variables = as.character(variables))

  vip = boot_vars_plotdf %>%
    ggplot2::ggplot(aes(x = penalty,
               y = boot_select_prob,
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
    ggplot2::labs(
      title = "Variable inclusion plot",
      x = "penalty",
      y = "empirical variable inclusion probability") +
    ggplot2::theme_classic(18) +
    ggplot2::theme(legend.position = "none")



  result = list(boot_vars_plotdf = boot_vars_plotdf, vip = vip)


  return(result)
}

get_opt_vars = function(
  penalty,
  apes_model_df,
  apes_mle_beta_binary){

  ## Get likeihood and model size to construct GIC
  log_like = apes_model_df$apes_mle_loglike
  model_size = apes_model_df$model_size

  ## Construct the grip of values
  penalty_grid = purrr::map(penalty, ~ -2*log_like + .x * model_size) %>%
    do.call(cbind, .)

  ## Find the minimum using GIC, for each penalty
  optimum_index = penalty_grid %>% apply(2, which.min)

  ## Find the models selected at each penalty
  opt_model_tbl = dplyr::mutate(
    apes_model_df[optimum_index,] %>%
      dplyr::select(
        .data$model_name,
        .data$model_size,
        .data$apes_mle_loglike),
    penalty)

  ## Find the variables corresponding to each selected model under GIC
  opt_beta_binary = purrr::map(
    opt_model_tbl$model_name,
    ~ dplyr::filter(apes_mle_beta_binary, model_name == .x)
  )
  names(opt_beta_binary) = round(penalty, 5)

  ## Output the optimum variable data frame
  opt_vars_tbl = opt_beta_binary %>%
    dplyr::bind_rows(.id = "penalty") %>%
    dplyr::mutate(penalty = as.numeric(penalty))

  return(opt_vars_tbl)
}
