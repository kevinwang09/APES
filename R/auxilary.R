#' Extract model elements (from mplot package)
#'
#' This function extracts things like the formula,
#' data matrix, etc. from a glm object
#'
#' @param model a fitted 'full' model, the result of a call to glm
#' @noRd
#' @references mplot
mextract = function(model){
  # what's the name of the dependent variable?
  yname = deparse(stats::formula(model)[[2]])
  # Set up the data frames for use
  data = stats::model.frame(model)
  x = stats::model.matrix(model)
  y = model$y
  n = nrow(x)
  p = ncol(x)
  ## Names of all variables, excluding (Intercept)
  variable_names = names(model$coefficients)[names(model$coefficients) != "(Intercept)"]
  variable_names = c("intercept", variable_names)
  family = stats::family(model)
  model_type = family$family
  fitted_values = model$fitted.values

  return(list(
    y = y,
    x = x,
    n = n,
    p = p,
    variable_names = variable_names,
    model_type = model_type,
    fitted_values = fitted_values))
}
