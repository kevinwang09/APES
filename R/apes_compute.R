#' @importFrom purrr map_dbl
apes_compute = function(x, y, fitted_values, linear_predictors, variable_names, k, estimator = "leaps", model_type, time_limit = 10, verbose = FALSE){
  if(model_type == "binomial"){
    linear_y = logit(fitted_values) + (y - fitted_values)/(fitted_values * (1 - fitted_values))
  } else if(model_type == "poisson"){
    linear_y = log(fitted_values) + (1/fitted_values)*(y - fitted_values)
  } else if(model_type == "coxph"){
    linear_y = linear_predictors
  }

  n = nrow(x)
  p = ncol(x)

  if(estimator == "leaps"){
    t1 = Sys.time()
    leaps_result = leaps::regsubsets(x = x, y = linear_y,
                                     really.big = TRUE,
                                     nbest = 1,
                                     nvmax = max(k),
                                     method = "exhaustive")
    t2 = Sys.time()
    time_used = difftime(t2, t1, units = "mins")

    if(verbose){
      cat("Finished solving linear regression approximation \n")
      ## The computational time
      print(time_used)
    }

    ############## Tidying up result from leaps object ###############
    ## Summary of the leaps object
    summ_leaps_result = summary(leaps_result)
    summ_leaps_result_which = summ_leaps_result$which
    ## Remove the intercept term
    apes_indicator = t(summ_leaps_result_which[, -which(colnames(summ_leaps_result_which) == "(Intercept)"), drop = FALSE])
    rownames(apes_indicator) = variable_names[-1]
    ## We always include intercept term!!
    apes_model_size = unname(colSums(apes_indicator) + 1L)
    ########################### End tidying #########################
    status = "leaps_optimal"
  } ## End estimator == "leaps"

  if(estimator == "mio"){

    if (!requireNamespace("bestsubset", quietly = TRUE)) {
      stop("Package \"bestsubset\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    t1 = Sys.time()
    mio_result = bestsubset::bs(x = x,
                                y = linear_y,
                                intercept = TRUE,
                                k = k,
                                verbose = TRUE,
                                tol = 1e-6,
                                form = 1,
                                nruns = 50,
                                time.limit = time_limit)
    t2 = Sys.time()

    time_used = difftime(t2, t1, units = "mins")

    if(verbose){
      cat("Finished solving linear regression approximation \n")
      print(time_used)
    }
    ############## Tidying up result from mio object ###############
    apes_hosmer_beta = mio_result$beta
    apes_indicator = (apes_hosmer_beta != 0)
    ## We always include intercept term!!
    apes_model_size = unname(colSums(apes_indicator) + 1L)
    ########################### End tidying #########################
    status = mio_result$status
  } ## End estimator == "mio"


  apes_model_name = paste0("apes_model_", apes_model_size)

  ########################## Distribution-specific re-fitting ######################################
  if(model_type == "binomial"){
    apes_mle_models = apply(apes_indicator, 2, function(indicator){
      refittingMle_logit(indicator = indicator, X = x, yBinom = y)
    })

    apes_mle_beta = mleModelToBeta(mleModels = apes_mle_models, variables = variable_names)
    colnames(apes_mle_beta) = apes_model_name

    ## theta is the transformation on the eta = x %*% beta, for logit, this is Pi = prob
    apes_mle_theta = apply(apes_mle_beta, 2, function(beta){beta2Pi(X = x, beta = beta)})
    apes_mle_loglike = apply(apes_mle_theta, 2, function(mlePi){loglikePi(yBinom = y, pis = mlePi)})
    apes_mle_bic = -2*apes_mle_loglike + log(n)*apes_model_size
    apes_mle_aic = -2*apes_mle_loglike + 2*apes_model_size
  } else if(model_type == "poisson"){
    apes_mle_models = apply(apes_indicator, 2, function(indicator){
      refittingMle_poisson(indicator = indicator, X = x, yPois = y)
    })

    apes_mle_beta = mleModelToBeta(mleModels = apes_mle_models, variables = variable_names)
    colnames(apes_mle_beta) = apes_model_name

    ## theta is the transformation on the eta = x %*% beta, for poisson, this is mu = mean
    apes_mle_theta = apply(apes_mle_beta, 2, function(beta){beta2Mu(X = x, beta = beta)})
    apes_mle_loglike = apply(apes_mle_theta, 2, function(mleMu){loglikeMu(yPois = y, mus = mleMu)})
    apes_mle_bic = -2*apes_mle_loglike + log(n)*apes_model_size
    apes_mle_aic = -2*apes_mle_loglike + 2*apes_model_size
  } else if(model_type == "coxph"){
    apes_mle_models = apply(apes_indicator, 2, function(indicator){
      refitting_cox(indicator = indicator, x = x, y = y)
    })

    apes_mle_beta = mleModelToBeta(mleModels = apes_mle_models, variables = variable_names)
    colnames(apes_mle_beta) = apes_model_name


    apes_mle_loglike = purrr::map_dbl(apes_mle_models, stats::logLik)
    apes_mle_bic = -2*apes_mle_loglike + log(n)*apes_model_size
    apes_mle_aic = -2*apes_mle_loglike + 2*apes_model_size
  }

  apes_model_df = tibble::tibble(
    model_name = apes_model_name,
    model_size = apes_model_size,
    ic_opt_models = paste0(
      icOptimal(ic = apes_mle_aic, "apes_min_aic"),
      icOptimal(ic = apes_mle_bic, "apes_min_bic")),
    apes_mle_loglike = apes_mle_loglike,
    mle_aic = apes_mle_aic,
    mle_bic = apes_mle_bic,
    status = status)
  ########################## End distribution-specific re-fitting ######################################
  ######################### Observation tibble #########################
  obs_num = paste0("obs", 1:n)
  # colnames(apes_mle_theta) = apes_model_name
  # rownames(apes_mle_theta) = obs_num

  # apes_min_aic_mean = minIcMatrix(ic = apes_mle_aic, mat = apes_mle_theta)
  # apes_min_bic_mean = minIcMatrix(ic = apes_mle_bic, mat = apes_mle_theta)

  response_tibble = tibble::tibble(obs_num = obs_num,
                                   y = y,
                                   fitted_values = fitted_values,
                                   linear_y = linear_y
                                   # apes_min_aic_mean = apes_min_aic_mean,
                                   # apes_min_bic_mean = apes_min_bic_mean
                                   )

  apes_mle_beta_binary = reshape2::melt(apes_mle_beta != 0,
                                        varnames = c("variables", "model_name"),
                                        value.name = "fitted_beta") %>% tibble::as_tibble()

  selected_model_beta = cbind(apes_min_aic = minIcMatrix(ic = apes_mle_aic, mat = apes_mle_beta),
                              apes_min_bic = minIcMatrix(ic = apes_mle_bic, mat = apes_mle_beta))

  ######################### End observation tibble #########################
  ####################### Model averaging ######################
  scale_apes_mle_aic = apes_mle_aic - min(apes_mle_aic)
  aic_weights = matrix(exp(-0.5*scale_apes_mle_aic)/sum(exp(-0.5*scale_apes_mle_aic)),
                       ncol = 1, byrow = TRUE)

  aic_weight_coef = apes_mle_beta %*% aic_weights

  scale_apes_mle_bic = apes_mle_bic - min(apes_mle_bic)
  bic_weights = matrix(exp(-0.5*scale_apes_mle_bic)/sum(exp(-0.5*scale_apes_mle_bic)),
                       ncol = 1, byrow = TRUE)
  bic_weight_coef = apes_mle_beta %*% bic_weights

  model_avg_beta = cbind(aic_weight_coef, bic_weight_coef)

  colnames(model_avg_beta) = c("aic_weight_coef", "bic_weight_coef")
  ####################### End model averaging ######################

  result = list(
    apes_model_df = apes_model_df,
    apes_mle_beta = apes_mle_beta,
    apes_mle_beta_binary = apes_mle_beta_binary,
    time_used = time_used,

    selected_model_beta = selected_model_beta,
    model_avg_beta = model_avg_beta,
    response_tibble = response_tibble)
  class(result) = "apes"
  return(result)
}
