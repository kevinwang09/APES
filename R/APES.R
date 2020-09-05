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
#' @param verbose Whether to print off messages duringcomputations
#' @import leaps
#' @import tibble
#' @importFrom magrittr %>%
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
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#'
#' model = glm(y ~ ., data = data, family = "binomial")
#' APES(model = model)

APES <- function(model, k = NULL, estimator = "leaps", time_limit = 10L, verbose = FALSE){
  extracts = mextract(model = model)
  x = extracts$x
  y = extracts$y
  n = extracts$n
  p = extracts$p
  model_type = extracts$model_type
  fitted_values = extracts$fitted_values

  if(model_type == "binomial"){
    linear_y = logit(fitted_values) + (y - fitted_values)/(fitted_values * (1 - fitted_values))
  } else if(model_type == "poisson"){

  }

  result = list(
    linear_y = linear_y
  )
  class(result) = "APES"
  return(result)
}

print.APES = function(x, ...) {
  cat("This is the result \n")
  print(x$linear_y)
}
