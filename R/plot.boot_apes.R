#' @title Generic plotting function for class \code{boot_apes}
#' @param x An object of class \code{boot_apes}
#' @param type Type of plot:
#' \itemize{
#' \item{vip}: Variable inclusion plot in Tarr et. al. 2018.
#' Shows the probability of variability selection across various penalty terms.
#' Either "AIC" or "BIC" can be shown using the order argument.
#' \item{"vi_tile" (default)}: Similar to "vi", but in a tile format.
#' \item{"path"}: Information criterion vs model size.
#' Either "AIC" or "BIC" can be shown using the order argument.
#' \item{"ma"}: Model averaged coefficient across bootstrap runs.
#' The weighted averages can be calculated from either "AIC" or "BIC" using the order argument.
#' }
#' @param max_vars Maximum number of variables to label. Default to NULL which plots all variables.
#' @param ... Additional parameters. Some options are:
#' \itemize{
#' \item{"order"}: Either "AIC", "BIC". If type is selected to be "vi_tile", then also takes the value "median".
#' }
#' @rdname plot.boot_apes
#' @return A ggplot output corresponding to the select plotting type.
#' @importFrom graphics plot
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
#' boot_result = apes(model = model, n_boot = 20)
#'
#' plot(boot_result, type = "vip_tile")
#' plot(boot_result, type = "vip")
#' plot(boot_result, type = "path")
#' plot(boot_result, type = "ma")
plot.boot_apes = function(x,
                          type = c("vip_tile", "vip", "path", "ma"),
                          max_vars = NULL, ...){
  type = match.arg(type)

  switch(type,
         vip_tile = plot_boot_apes_vip_tile(x = x, ...),
         vip = plot_boot_apes_vip(x = x, ...),
         path = plot_boot_apes_path(x = x, ...),
         ma = plot_boot_apes_ma(x = x, ...))
}
