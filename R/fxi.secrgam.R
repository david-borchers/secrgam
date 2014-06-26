
#' @title Probability Density of Home Range Centre
#'   
#' @description Calculates the probability density function for the estimated 
#'   location of one or more range centres, i.e. \eqn{f(X | \Omega)}. Acts as a 
#'   wrapper function for \code{\link{fxi.secr}}.
#'   
#' @param fit a fitted secrgam model (returned from 
#'   \code{\link{secrgam.fit}})
#' @param i integer or character vector of individuals for which to plot contours
#' @param col integer or character vector of colour for individuals' contours
#' @param X 2-column matrix of x- and y- coordinates (e.g. the original mask)
#' @param plt logical to do contour plot
#' @param add logical to add to existing plot
#' @param sessnum session number if \code{fit$capthist} spans multiple sessions
#' @param border width of blank margin around the outermost detectors
#' @param normal logical; should values of pdf be normalised?
#' @param ... additional arguments passed to contour.
#'   
#' @details \code{fxi.secr} returns a vector of probability densities assuming a
#'   uniform density distribution,
#'   
#'   \deqn{f(x_i | \omega_i) = P(\omega_i | x_i) / \sum P(\omega_i | x_i)}
#'   
#'   \code{fxi.secrgam} returns a vector of probability densities for any
#'   density distribution,
#'   
#'   \deqn{f(x_i | \omega_i) = P(\omega_i | x_i) D(x_i) / \sum [ P(\omega_i |
#'   x_i) D(x_i) ]}
#'   
#'   Needs to be tested for cases where density model has fixed parameters. Also assumes that log link is used for density.
#' @export
#' @examples
#' data(Boland.fits1)
#' 
#' # capture history data
#' plot(fit1.a3$capthist)
#' 
#' # look at data for animal 20 
#' animal = 20
#' k = apply(fit1.a3$capthist[animal,,], 2, sum) > 0
#' points(traps(fit1.a3$capthist)[k,], col = 2, pch = 19)
#' fxi.secrgam(fit1.a3, i = animal, add = TRUE)

fxi.secrgam = function (fit, i=1, col=1:length(i),X = NULL, plt=TRUE, add=FALSE, sessnum=1, border=0, normal=TRUE, ...) {

  # if no X supplied, then use the original mask
  if(is.null(X)) X = fit$mask
  
  # if X is a mask, then convert it to a model frame
  if(inherits(X, "mask"))
    X = cbind(X, attributes(X)$covariates)
    
  # persuade predict.secr to give you predictions
#   D = predict(fit, X, se = FALSE)[,"D"] ; D # way too slow...
  D = exp(as.numeric(model.matrix(fit$model$D, X) %*% fit$fit$par[fit$parindx$D])) ; D
#   D = attributes(predictDsurface(fit))$covariates[,"D.0"] ; D

  # modify the density model so that fxi.secr doesn't throw an error
  fit$model$D = ~1
  
  ni=length(i)
  fxi=matrix(rep(NA,ni*length(D)),ncol=ni)
  for(indiv in 1:ni){
    # get the normalised version of P(Omega | X) from the fxi.secr function
    fxi[,indiv] = fxi.secr(object = fit, i=i[indiv], X = X[,c("x","y")], ...)
    
    # modify this to incorporate the fitted density surface
    # (the normalising constant used in fxi.secr cancels out)
    fxi[,indiv] = fxi[,indiv] * D / sum(fxi[,indiv] * D)
    
    if(plt){
      ctdf=data.frame(x=X$x,y=X$y,z=fxi[,indiv])
      contour(prep4image(ctdf,plot=FALSE),add=add,col=col[indiv],...)
    }
    if(!add) add=TRUE # so that subsequent contours are on same plot
  }
  if (plt) invisible(fxi)
  else return(fxi)

  # Note - it would probably be a bit quicker to use:
  # fxi = D * fxi.secr(object = fit, X = X, ...)
  # return(fxi / sum(fxi))
  # but the code used is a bit more readable
  
}
