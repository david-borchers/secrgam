
#' @title Generate a population using any density formula
#'   
#' @description description...
#'   
#' @param Dmodel density model formula
#' @param Dpars associated parameters for the density model
#' @param mask \code{\link{mask}} object (as used by \code{\link{secrgam.fit}} 
#'   and \code{\link{secr.fit}})
#' @param link if \code{TRUE} the log link is used, if \code{FALSE} the identity link is used
#'   
#' @details details...
#' @return value...
#' @examples
#' # construct a tensor product model to represent a bivariate normal
#' data(Boland.leopards)
#' require(mvtnorm)
#' D = dmvnorm(Boland.mask, apply(Boland.mask, 2, mean), 10e7*diag(2)) * 10e6
#' N = sum(D) * attributes(Boland.mask)$area ; N
#' data = cbind(z = D, Boland.mask)
#' prep4image(data, key = FALSE, col = tim.colors(10))
#' Dmodel = D ~ te(x, y, k = 3)
#' fit = gam(Dmodel, gaussian(link = "log"), data)
#' prep4image(data.frame(x = data$x, y = data$y, z = fitted(fit)), key = FALSE)
#' Dpars = coef(fit) ; Dpars
#' 
#' # simulate a population
#' popn = sim.popn.secrgam(Dmodel, Dpars, Boland.mask) ; head(popn) ; dim(popn)
#' points(popn, cex = 0.5, pch = 19)
#' @importFrom mgcv gam
#' @export

sim.popn.secrgam = function(Dmodel, Dpars, mask, log = TRUE){
  
  # make dummy data for use in gam
  data = cbind(D = 1, mask, attributes(mask)$covariates)
  
  # use gam to get design matrix
  X = gam(Dmodel, gaussian(link = if(log) "log" else "identity"), data, fit = FALSE)$X
  
  # evaluate the model for each mask point
  D = X %*% Dpars
  if(log) D = exp(D)
  
  # use the IHP option in sim.popn
  sim.popn(D, mask, model2D = "IHP", covariates = NULL) 
    
}



