#' @title Perform APES model selection
#' @description Approximated exhaustive selection will be performed on the input GLM model.
#' @details This is the main function of the APES package. Note that the feature dimensions
#' equals to the number of columns if all columns are numeric.
#' If factor/categorical variables are present in the input "glm" object,
#' then the feature dimension also includes all the levels of factors. See vignette on birth weight.
#' @param model A "full" model with all variables of interest fitted to it.
#' Accepts either a "glm" class or "coxph" class object.
#' @param estimator Either "leaps" (default) or "mio", which correspond to
#' optimisation algorithms available in
#' the leaps and bestsubset package, respectively.
#' @param k Model size to explore. Default to NULL, which searches through all variables in the model.
#' Alternatively, user can input a vector. If estimator is:
#' \itemize{
#'  \item "leaps", then models up to size max(k) will be explored.
#'  \item "mio", then models with specified values will be explored.
#' }
#' @param fitted_values Default to NULL, and fitted values are extracted from the fitted model itself.
#' However, users can specify a vector of fitted values to
#' improve the performance of APES as per Wang et. al. 2019.
#' @param really_big If set to FALSE (by default), then it will prevent variable selection greater than 30 variables.
#' We recommend only setting this argument to TRUE if the number of variables exceed 30 while simultaneous setting the
#' "estimator" argument to "mio".
#' @param time_limit The time limit for the maximum time allocated to each
#' model size model when the "mio" estimator was selected. It will not affect the computational speed if "leaps" is selected as the estimator.
#' @param verbose Whether to print off messages during computations.
#' @param n_boot Number of bootstrap runs, default to 0, which doesn't perform any sampling.
#' @param workers Number of cores used for parallel processing for the bootstrap
#' @return Either an object of class "apes" in case "n_boot" is set to zero or
#' an object of class "boot_apes" in case "n_boot" is set to a positive integer.
#' @importFrom tibble tibble
#' @import furrr
#' @import future
#' @importFrom rlang .data
#' @importFrom survival coxph Surv
#' @importFrom dplyr %>%
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' beta = c(5, -5, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#'
#' ## Logistic regression example
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "binomial")
#' apes_result = apes(model = model)
#' apes_result
#' class(apes_result)
#' names(apes_result)
#'
#' ## Poisson regression example
#' y = rpois(n = n, lambda = exp(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "poisson")
#' apes(model = model)
#'
#' ## Bootstrap examples
#' apes(model = model, n_boot = 2)
#' apes(model = model, n_boot = 2, workers = 1)
#' ## apes(model = model, n_boot = 2, workers = 2)
#'
#' ## Cox regression example
#' hx = exp(x %*% beta)
#' time = rexp(n, hx)
#' censor = rbinom(n = n, prob = 0.3, size = 1) # censoring indicator
#' data = data.frame(x, time = time, censor = censor)
#' model = survival::coxph(survival::Surv(time, censor) ~ ., data = data)
#' apes(model = model)
apes <- function(model, k = NULL, estimator = "leaps",
                 fitted_values = NULL,
                 time_limit = 10L, really_big = FALSE,
                 verbose = FALSE,
                 n_boot = 0, workers = 1L){

  extracts = mextract(model = model)
  x = extracts$x
  y = extracts$y
  n = extracts$n
  p = extracts$p
  model_type = extracts$model_type
  variable_names = extracts$variable_names
  linear_predictors = extracts$linear_predictors

  ## If the user specifies some fitted values, we will use that to compute weights.
  if(is.null(fitted_values)){
    fitted_values = extracts$fitted_values
  }

  if(p >= 30 & !really_big){
    message("p (including factors) is ")
    stop("p is too large! If you want to proceed, you need to set 'reall_big' to TRUE.")
  }

  list_boot_index = base::replicate(
    n = n_boot,
    expr = sample(seq_len(nrow(x)), replace = TRUE),
    simplify = FALSE)

  ## Determine k to search through
  if(is.null(k)){
    message("No variable size specified, searching all sizes from 1 to p... \n")
    k = seq_len(p)
  }

  if(n_boot == 0){
    result = apes_compute(
      x = x,
      y = y,
      fitted_values = fitted_values,
      linear_predictors = linear_predictors,
      variable_names = variable_names,
      k = k,
      estimator = estimator,
      time_limit = time_limit,
      model_type = model_type,
      verbose = verbose)
  } else { ## If boostrap is invoked

    list_boot_index = base::replicate(
      n = n_boot,
      expr = sample(seq_len(n), replace = TRUE),
      simplify = FALSE)

    t1 = Sys.time()
    if(workers > 1L) {
      future::plan(multisession, workers = workers)
      result = furrr::future_map(
        .x = list_boot_index,
        .f = ~ apes_compute(x = x[.x,],
                            y = y[.x],
                            fitted_values = fitted_values,
                            linear_predictors = linear_predictors,
                            variable_names = variable_names,
                            k = k,
                            estimator = estimator,
                            time_limit = time_limit,
                            model_type = model_type,
                            verbose = verbose),
        .progress = FALSE)
      future::plan(sequential)
    } else {
      result = purrr::map(
        .x = list_boot_index,
        .f = ~ apes_compute(x = x[.x,],
                            y = y[.x],
                            fitted_values = fitted_values,
                            linear_predictors = linear_predictors,
                            variable_names = variable_names,
                            k = k,
                            estimator = estimator,
                            time_limit = time_limit,
                            model_type = model_type,
                            verbose = verbose))
    }
    names(result) = paste0("boot_num", seq_len(n_boot))
    class(result) = "boot_apes"
    t2 = Sys.time()
    attr(result, "time_used") = as.numeric(difftime(t2, t1, units = "mins"))

  }
  return(result)
}

#' @export
print.apes = function(x, ...) {
  cat("Time taken: ", x$time_used, " minutes \n")

  cat("\n APES - AIC selected the following variables \n")
  print(round(x$selected_model_beta[,"apes_min_aic"], 3))

  cat("\n APES - BIC selected the following variables \n")
  print(round(x$selected_model_beta[,"apes_min_bic"], 3))
}

#' @export
print.boot_apes = function(x, ...) {
  cat("Time taken: ", attr(x, "time_used"), " minutes \n")
  cat("Total number of bootstrap APES results: ", length(x))
}

#' @export
summary.boot_apes = function(object, ...){
  cat("Summary of top variables selected by AIC and BIC: \n")
  print(
    dplyr::inner_join(
      dplyr::select(apes_var_freq(list_result = object, ic = "BIC"),
                    .data$variables, bic_freq = .data$freq),
      dplyr::select(apes_var_freq(list_result = object, ic = "AIC"),
                    .data$variables, aic_freq = .data$freq),
      by = "variables"))
}


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
