#' @title Generic plotting function for class \code{apes}
#' @param x An object of class \code{apes}
#' @param type Type of plot: (only supporting "ic" at the moment)
#' \itemize{
#' \item{"path"}: Information criterion vs model size.
#' Either "AIC" or "BIC" can be shown using the order argument.
#' }
#' @param ... additional parameters (not currently used)
#' @rdname plot.apes
#' @return A ggplot output corresponding to the select plotting type.
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' beta = c(1, -1, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "binomial")
#'
#' apes_result = apes(model = model)
#' plot(apes_result)
plot.apes = function(x, type = "path", ...){
  type = match.arg(type)

  switch(type,
         path = plot_path_boot_apes(x = list(x), ...))
}
