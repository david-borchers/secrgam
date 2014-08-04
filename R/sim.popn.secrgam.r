#' @title Generate a population using any density formula
#'   
#' @description description...
#'   
#' @param fit a fitted \code{\link{secrgam}} or \code{\link{secr}} model
#' @param N average abundance (converts density surface to relative density)
#' @param Dmodel density model formula
#' @param Dpars associated parameters for the density model
#' @param Dlink link function (default is "log")
#' @param mask \code{\link{mask}} object (as used by \code{\link{secrgam.fit}} 
#'   and \code{\link{secr.fit}})
#' @param log if \code{TRUE} the log link is used, if \code{FALSE} the identity
#'   link is used
#'   
#' @details details...
#' @return value...
#' @examples
#' data(Boland.leopards1)
#' data(Boland.fits1)
#' 
#' # use a fitted model
#' popn = sim.popn.secrgam(fit1.a3) ; head(popn) ; dim(popn)
#' plot(fit1.a3, type = "density", asp = 1)
#' points(popn, cex = 0.5, pch = 19)
#' 
#' # use an inflated density surface from a fitted model
#' popn = sim.popn.secrgam(fit1.a3, N = 1000) ; head(popn) ; dim(popn)
#' plot(fit1.a3, type = "density", asp = 1)
#' points(popn, cex = 0.5, pch = 19)
#' 
#' \dontrun{
#' # use an alternative model
#' # e.g. construct a tensor product model to represent a bivariate normal
#' require(mvtnorm)
#' D = dmvnorm(Boland.mask1, apply(Boland.mask, 2, mean), 10e7 * diag(2)) * 10e6
#' N = sum(D) * attributes(Boland.mask)$area ; N # expected sample size
#' data = cbind(z = D, Boland.mask)
#' prep4image(data, key = FALSE, col = topo.colors(10), asp = 1)
#' model = D ~ te(x, y, k = 3)
#' fit = gam(model, gaussian(link = "log"), data, fx = TRUE)
#' prep4image(data.frame(x = data$x, y = data$y, z = fitted(fit)), key = FALSE, asp = 1)
#' popn = sim.popn.secrgam(Dmodel = model, Dpars = coef(fit), mask = Boland.mask1) ; head(popn) ; dim(popn)
#' points(popn, cex = 0.5, pch = 19)
#' }
#' @importFrom mgcv gam
#' @export

sim.popn.secrgam = function(fit = NULL, N = NULL, Dmodel = NULL, Dpars = NULL, Dlink = "log", mask = NULL){

  # if no mask supplied use original mask
  if(is.null(mask)) mask = fit$orig.mask
  
  # if a fitted model is supplied, then extract Dmodel, Dpars, Dlink and mask
  if(!is.null(fit)){
    
    D = fitted(fit, mask)
    
  }else{
    
    # fitted model not supplied
    if(is.null(Dmodel) || is.null(Dpars) || is.null(mask)) 
      stop("'Dmodel', 'Dpars' and 'mask' must all be supplied if a fitted model is not given")
    
    if(!inherits(mask, "mask")) stop("'mask' must be a mask object")
    
    # make dummy data for use in gam
    data = cbind(D = 1, mask, attributes(mask)$covariates)
    
    # use gam to get the design matrix
    X = gam(Dmodel, gaussian(link = Dlink), data, fx = TRUE, fit = FALSE)$X
    
    # evaluate the model for each mask point
    D = switch(
      Dlink, 
      "identity" = X %*% Dpars,
      "log" = exp(X %*% Dpars),
      stop("'Dlink' not recognsed - only log and idenity links implemented")
    )
    
  }
  
  # inflate D so that E[N] = N
  if(!is.null(N)) D = D * N / sum(D * attr(mask, "area"))
  
  # use the IHP option in sim.popn
  sim.popn(D, mask, model2D = "IHP", covariates = NULL) 
  
}



