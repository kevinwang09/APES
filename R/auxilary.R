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

  ## Names of all variables, excluding (Intercept)
  variable_names = names(model$coefficients)[names(model$coefficients) != "(Intercept)"]
  x = x[, variable_names, drop = FALSE]
  n = nrow(x)
  p = ncol(x)
  ## We change (Intercept) to intercept
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

icOptimal = function(ic, symbol){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = rep("", length(ic))
    res[which.min(ic)] = symbol
  }
  return(res)
}
######################################
## Calculate the log-likelihood of logisitic regression using yBinom and estimated pis
loglikePi = function(yBinom, pis){
  loglike = yBinom*log(pis) + (1-yBinom)*log(1-pis)
  return(sum(loglike))
}
#####################################
## Calculate estimated pis using beta
beta2Pi = function(X, beta){
  xint = cbind(intercept = 1,X)
  expit(xint[, names(beta)] %*% as.matrix(beta))
}
#####################################
## Given an indicators of variables, design matrix and the yBinom, we refit the MLE-logisitic. X should not have an Intercept term
refittingMle_logit = function(indicator, X, yBinom){
  xTemp = cbind(intercept = 1, X[,indicator])
  colnames(xTemp) = c("intercept", colnames(X)[indicator])

  stats::glm.fit(x = xTemp,
                 y = yBinom,
                 # etastart = fullModel$linear.predictors,
                 family = stats::binomial(link = "logit"))
}
#####################################
icOptimal = function(ic, symbol){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = rep("", length(ic))
    res[which.min(ic)] = symbol
  }
  return(res)
}
#####################################
minIcMatrix = function(ic, mat){
  if(length(which.min(ic)) == 0){
    res = NA
  } else {
    res = mat[,which.min(ic)]
  }
  return(res)
}
#####################################
mleModelToBeta = function(mleModels, variables){
  mleBeta = purrr::map(mleModels, "coefficients") %>%
    purrr::map(t) %>%
    purrr::map(data.frame) %>%
    dplyr::bind_rows() %>% t

  mleBeta[is.na(mleBeta)] = 0L
  variablesOrdered = base::intersect(rownames(mleBeta), variables) %>% gtools::mixedsort()
  mleBeta = mleBeta[variablesOrdered,,drop = FALSE]

  tmp = matrix(0L,
               nrow = length(variables),
               ncol = ncol(mleBeta),
               dimnames = list(variables, colnames(mleBeta))) ## To avoid a variable is never selected, we create an empty matrix

  tmp[rownames(mleBeta), ] = mleBeta

  return(tmp)
}
####################################
## Calculate the log-likelihood of logisitic regression using yPois and estimated pis
loglikeMu = function(yPois, mus){
  # loglike = yPois*log(pis) + (1-yPois)*log(1-pis)
  loglike = -mus + yPois * log(mus) - log(factorial(yPois))
  return(sum(loglike))
}
#####################################
## Calculate estimated pis using beta
beta2Mu = function(X, beta){
  xint = cbind(intercept = 1,X)
  exp(xint[, names(beta)] %*% as.matrix(beta))
}
#####################################
## Given an indicators of variables, design matrix and the yPois, we refit the MLE-logisitic. X should not have an Intercept term
refittingMle_poisson = function(indicator, X, yPois){
  xTemp = cbind(intercept = 1, X[,indicator])
  colnames(xTemp) = c("intercept", colnames(X)[indicator])

  stats::glm.fit(x = xTemp,
                 y = yPois,
                 # etastart = fullModel$linear.predictors,
                 family = stats::poisson(link = "log"))
}
