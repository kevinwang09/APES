#' @title Perform APES model selection
#' @param model A "glm" class object
#' @param estimator Either "leaps" (default) or "mio", which correspond to
#' optimisation algorithms available in
#' the leaps and bestsubset package, respectively.
#' @param k Model size to explore. Default to NULL, which searches through all variables in the model.
#' Alternatively, user can input a vector. If estimator is:
#' \itemize{
#'  \item "leaps", then models up to size max(k) wil be explored.
#'  \item "mio", then models with specified values will be explored.
#' }
#' @param time_limit The time limit for the maximum time allocated to each
#' model size model when the "mio" estimator was selected. It will not affect the speed if leaps.
#' @param verbose Whether to print off messages during computations
#' @param n_boot Number of bootstrap runs, default to 0, which doesn't perform any sampling.
#' @param workers Number of cores used for parallel processing for the bootstrap
#' @import leaps
#' @import tibble
#' @import furrr
#' @import future
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom survival coxph Surv
#' @import dplyr
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' k = 1:10
#' beta = c(1, -1, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#'
#' ## Logistic regression example
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "binomial")
#' apes(model = model)
#'
#' ## Poisson regression example
#' y = rpois(n = n, lambda = exp(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "poisson")
#' apes(model = model)
#'
#' ## Bootstrap examples
#' apes(model = model, n_boot = 2
#' apes(model = model, n_boot = 2, workers = 1)
#' apes(model = model, n_boot = 2, workers = 2)
#'
#' ## Cox regression example
#' hx = exp(x %*% beta)
#' time = rexp(n, hx)
#' censor = rbinom(n = n, prob = 0.3, size = 1) # censoring indicator
#' data = data.frame(x, time = time, censor = censor)
#' model = survival::coxph(survival::Surv(time, censor) ~ ., data = data)
#' apes(model = model)
apes <- function(model, k = NULL, estimator = "leaps", time_limit = 10L, verbose = FALSE,
                 n_boot = 0L, workers = 1L){

  extracts = mextract(model = model)
  x = extracts$x
  y = extracts$y
  n = extracts$n
  p = extracts$p
  model_type = extracts$model_type
  fitted_values = extracts$fitted_values
  variable_names = extracts$variable_names
  linear_predictors = extracts$linear_predictors


  list_boot_index = base::replicate(
    n = n_boot,
    expr = sample(seq_len(nrow(x)), replace = TRUE),
    simplify = FALSE)

  ## Determine k to search through
  if(is.null(k)){k = seq_len(p)}

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
    if(workers >= 1) {
      plan(multisession, workers = workers)
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
      plan(sequential)
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

print.apes = function(x, ...) {
  cat("Time taken: \n")
  print(x$time_used)

  cat("\n APES model selection data frame: \n")
  print(x$apes_model_df)
}

print.boot_apes = function(x, ...) {
  cat("Time taken: ", attr(x, "time_used"), " minutes \n")
  cat("Total number of bootstrap APES results: ", length(x))
}

summary.boot_apes = function(x, ...){
  cat("Summary of top variables selected by BIC: \n")
  print(apes_var_freq(list_result = x, ic = "BIC"))
}


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
