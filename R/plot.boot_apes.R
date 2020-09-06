#' Variable inclusion plot in tile format
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
#' @param order Either "AIC", "BIC".
#' @param ... additional parameters (not currently used)
#' @rdname plot.boot_apes
#' @return A ggplot output
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
#' plot(list_result, type = "vi_tile")
#' plot(list_result, type = "vi")
#' plot(list_result, type = "ic")
#' plot(list_result, type = "ma")
plot.boot_apes = function(x, type = c("vi_tile", "vi", "ic", "ma"), order = c("BIC", "AIC"), ...){
  type = match.arg(type)
  order = match.arg(order)

  switch(type,
         vi_tile = plot_vi_tile_boot_apes(list_result = x, order = order),
         vi = plot_vi_boot_apes(list_result = x),
         ic = plot_ic_boot_apes(list_result = x, order = order),
         ma = plot_ma_boot_apes(list_result = x, order = order))
}
