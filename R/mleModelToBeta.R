#' We take the MLE model fitted above and rearrange the columns and remove the NA's in our vector
#' @param mleModels mleModels
#' @param variables variable names
#' @export
#'
#'
mleModelToBeta = function(mleModels, variables){
  mleBeta = purrr::map(mleModels, "coefficients") %>%
    purrr::map(t) %>%
    purrr::map(data.frame) %>%
    dplyr::bind_rows() %>% t

  mleBeta[is.na(mleBeta)] = 0L
  variablesOrdered = intersect(rownames(mleBeta), variables) %>% gtools::mixedsort()
  # variablesOrdered = variables %>% mixedsort
  mleBeta = mleBeta[variablesOrdered,]

  tmp = matrix(0L,
               nrow = length(variables),
               ncol = ncol(mleBeta),
               dimnames = list(variables, colnames(mleBeta))) ## To avoid a variable is never selected, we create an empty matrix

  tmp[rownames(mleBeta), ] = mleBeta

  return(tmp)
}
