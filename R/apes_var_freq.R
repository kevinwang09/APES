#' @title Calculates the frequency that a variable is selected
#' @param list_result a list of APES outputs
#' @param ic Either "AIC" or "BIC"
#' @author Kevin Wang
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom purrr map_dfr
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
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "binomial")
#'
#' list_result = apes(model = model, n_boot = 50)
#'
#' apes_var_freq(list_result = list_result, ic = "AIC")
#' apes_var_freq(list_result = list_result, ic = "BIC")

apes_var_freq = function(list_result, ic = "BIC"){
  list_apes_model_df = purrr::map_dfr(list_result, "apes_model_df",  .id = "boot_num") %>%
    dplyr::mutate(boot_num_model_name = base::paste(boot_num, model_name, sep = "_"))

  list_apes_mle_beta_binary = purrr::map_dfr(list_result, "apes_mle_beta_binary",  .id = "boot_num") %>%
    dplyr::mutate(boot_num_model_name = base::paste(boot_num, model_name, sep = "_"))

  if(ic == "AIC"){
    ic_optimal_models = list_apes_model_df %>%
      dplyr::filter(stringr::str_detect(ic_opt_models, "apes_min_aic"))
  }

  if(ic == "BIC"){
    ic_optimal_models = list_apes_model_df %>%
      dplyr::filter(stringr::str_detect(ic_opt_models, "apes_min_bic"))
  }

  ic_optimal_variables = list_apes_mle_beta_binary %>%
    dplyr::filter(boot_num_model_name %in% ic_optimal_models$boot_num_model_name)

  ic_optimal_variables_freq = ic_optimal_variables %>%
    dplyr::group_by(variables) %>%
    dplyr::summarise(freq = mean(fitted_beta), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(freq)) %>%
    dplyr::ungroup()

  return(ic_optimal_variables_freq)
}
