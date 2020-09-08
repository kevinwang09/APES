#' @title Generic plotting function for class \code{boot_apes}
#' @param x An object of class \code{boot_apes}
#' @param type Type of plot:
#' \itemize{
#' \item{vi}: Variable inclusion plot in Tarr et. al. 2018.
#' Shows the probability of variability selection across various penalty terms.
#' Either "AIC" or "BIC" can be shown using the order argument.
#' \item{"vi_tile" (default)}: Similar to "vi", but in a tile format.
#' \item{"ic"}: Information criterion vs model size.
#' Either "AIC" or "BIC" can be shown using the order argument.
#' \item{"ma"}: Model averaged coefficient across bootstrap runs.
#' The weighted averages can be calculated from either "AIC" or "BIC" using the order argument.
#' }
#' @param order Either "AIC", "BIC". If type is selected to be "vi_tile", then also takes the value "median".
#' @param ... additional parameters (not currently used)
#' @rdname plot.boot_apes
#' @return A ggplot output corresponding to the select plotting type.
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
#' list_result = apes(model = model, n_boot = 20)
#'
#' plot(list_result, type = "vip_tile")
#' plot(list_result, type = "vip")
#' plot(list_result, type = "path")
#' plot(list_result, type = "ma")
plot.boot_apes = function(x, type = c("vip_tile", "vip", "path", "ma"), order = c("BIC", "AIC", "median"), ...){
  type = match.arg(type)
  order = match.arg(order)

  if(order == "median" & type == "path") {
    stop("Path plot does not accept 'median' as order input")
  }

  switch(type,
         vip_tile = plot_vip_tile_boot_apes(x = x, order = order),
         vip = plot_vip_boot_apes(x = x),
         path = plot_path_boot_apes(x = x, order = order),
         ma = plot_ma_boot_apes(x = x, order = order))
}