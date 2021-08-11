#' @title Converting APES object into mplot-vis object (experimental)
#' @param boot_apes Bootstrapped APES result
#' @param model The full model
#' If TRUE, then googleVis will be used to make an interactive plot.
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select %>%
#' @importFrom rlang .data
#' @export
#' @examples
#' set.seed(10)
#' n = 100
#' p = 10
#' beta = c(1, -1, rep(0, p-2))
#' x = matrix(rnorm(n*p), ncol = p)
#' colnames(x) = paste0("X", 1:p)
#'
#' y = rbinom(n = n, size = 1, prob = expit(x %*% beta))
#' data = data.frame(y, x)
#' model = glm(y ~ ., data = data, family = "binomial")
#' boot_apes = apes(model = model, n_boot = 20)
#' \dontrun{
#' library(mplot)
#' plot(x = as_vis(boot_apes = boot_apes, model = model), which = "vip")
#' plot(x = as_vis(boot_apes = boot_apes, model = model), which = "vip", interactive = TRUE)
#' }
as_vis = function(boot_apes, model){
  result = list()
  result$B = length(boot_apes)

  result$mf = model
  result$mextract = mplot_mextract(result$mf, redundant = FALSE)

  lambda.max = 2 * log(nrow(boot_apes[[1]]$response_tibble))
  lambdas = seq(0, lambda.max, 0.01)

  nms = base::all.vars(stats::formula(result$mf))
  nms = nms[nms != result$mextract$yname]
  real.k = length(nms) + 1

  var.in = matrix(NA, ncol = real.k, nrow = length(lambdas))
  colnames(var.in) = rownames(boot_apes[[1]]$selected_model_beta)
  list_apes_model_df = purrr::map(.x = boot_apes, "apes_model_df")
  list_apes_mle_beta_binary = purrr::map(
    .x = boot_apes,
    .f = function(x){x[["apes_mle_beta_binary"]] %>%
        tidyr::pivot_wider(names_from = .data$variables,
                           values_from = .data$fitted_beta) %>%
        dplyr::select(-.data$model_name)})
  rownames(var.in) = lambdas

  for (i in 1:length(lambdas)) {
    resl = lapply(list_apes_model_df, function(x)
      - 2 * x$apes_mle_loglike + lambdas[i] * x$model_size)
    min.pos = unlist(lapply(resl, which.min))
    temp.best = mapply(function(x, row) {
      x[row, 1:real.k]
    }, list_apes_mle_beta_binary, min.pos, SIMPLIFY = TRUE)
    #########################
    temp.best = matrix(
      as.numeric(temp.best),
      nrow = dim(temp.best)[1],
      dimnames = dimnames(temp.best)
    )
    var.in[i, ] = rowSums(temp.best)
  }

  result$var.in = var.in
  result$lambdas = lambdas

  class(result) = "vis"
  return(result)
}
################################
mplot_mextract = function(model, screen = FALSE, redundant = TRUE){
  # what's the name of the dependent variable?
  yname = deparse(stats::formula(model)[[2]])
  # Set up the data frames for use
  data = stats::model.frame(model)
  X = stats::model.matrix(model)
  n = nrow(X)
  # full model plus redundant variable
  exp.vars = names(model$coefficients)[names(model$coefficients) != "(Intercept)"]

  if (redundant) {
    REDUNDANT.VARIABLE = stats::runif(n, min = 0, max = 1)
    X = cbind(X,REDUNDANT.VARIABLE)
    data = cbind(data,REDUNDANT.VARIABLE)
    exp.vars = c(exp.vars,"REDUNDANT.VARIABLE")
  }
  if (colnames(X)[1] == "(Intercept)") {
    # overwrite intercept with y-variable
    X[,1] = stats::model.frame(model)[,yname]
  } else {
    X = cbind(stats::model.frame(model)[,yname],X)
  }
  colnames(X)[1] = yname
  X = data.frame(X)
  fixed = stats::as.formula(c(paste(yname, "~"),
                              paste(colnames(X)[-1], collapse = "+")))
  Xy = X[c(2:ncol(X),1)]

  k = length(exp.vars) + 1 # +1 for intercept
  # if (screen) {
  #   if (!requireNamespace("mvoutlier", quietly = TRUE)) {
  #     stop("mvoutlier package needed when screen=TRUE. Please install it.",
  #          call. = FALSE)
  #   }
  #   x.mad = apply(Xy, 2, stats::mad)
  #   Xy.sub = Xy[,which(x.mad != 0)]
  #   Xy = Xy[mvoutlier::pcout(Xy.sub)$wfinal01 == 1,]
  #   n = dim(Xy)[1]
  #   if (k >= n) {
  #     warning("Screening deleted too many observations.")
  #     return()
  #   }
  # }
  wts = model$weights
  if (is.element("glm",class(model))) {
    wts = model$prior.weights
    Xy[,yname] = model$y
  }
  if (is.null(wts)) {
    wts = rep(1,n)
  }

  return(list(yname = yname, fixed = fixed,
              wts = wts, X = Xy, k = k,
              n = n, exp.vars = exp.vars,
              data = data, family = stats::family(model)))
}
