
#' @title Probability Density of Home Range Centre
#'   
#' @description Calculates the probability density function for the estimated 
#'   location of one or more range centres, i.e. \eqn{f(X | \Omega)}. Acts as a 
#'   wrapper function for \code{\link{fxi.secr}}.
#'   
#' @param object a fitted secrgam model (returned from 
#'   \code{\link{secrgam.fit}})
#' @param X 2-column matrix of x- and y- coordinates (e.g. the original mask)
#' @param ... additional arguments passed to \code{\link{fxi.secr}} (i.e. 
#'   \code{i}, \code{sessnum} or \code{normal})
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

fxi.secrgam = function (object, X = NULL, ...) {

  # if no X supplied, then use the original mask
  if(is.null(X)) X = object$mask
  
  # if X is a mask, then convert it to a model frame
  if(inherits(X, "mask"))
    X = cbind(X, attributes(X)$covariates)
    
  # persuade predict.secr to give you predictions
#   D = predict(object, X, se = FALSE)[,"D"] ; D # way too slow...
  D = exp(as.numeric(model.matrix(object$model$D, X) %*% object$fit$par[object$parindx$D])) ; D
#   D = attributes(predictDsurface(fit))$covariates[,"D.0"] ; D

  # modify the density model so that fxi.secr doesn't throw an error
  object$model$D = ~1
  
  # get the normalised version of P(Omega | X) from the fxi.secr function
  fxi = fxi.secr(object = object, X = X[,c("x","y")], ...)
  
  # modify this to incorporate the fitted density surface
  # (the normalising constant used in fxi.secr cancels out)
  fxi = fxi * D / sum(fxi * D)
  
  return(fxi)

  # Note - it would probably be a bit quicker to use:
  # fxi = D * fxi.secr(object = object, X = X, ...)
  # return(fxi / sum(fxi))
  # but the code used is a bit more readable
  
}
