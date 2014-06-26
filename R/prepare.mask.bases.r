
#' @title Add spline basis functions to mask covariates.
#'   
#' @description Replaces attr(mask,"covariates") with a data frame containing
#' new variables comprising basis function values for smoopthed variables,
#' toghether with non-smoothed variables (but excluding "x" and "y", which are
#' in mask already). 
#' 
# Also adds a list of basis functions to
# attributes(mask)$bases, and the df of each basis in
# attributes(mask)$bases.df. The list attribtes(mask)$bases contains original
# variables as 1D matrix if the variable is not smoothed, and spline basis
# object (a >1D matrix with attributes givning knots, etc.) if it is smoothed. 
# The vector attr(mask,"bases.df") contains the degrees of freedom of the
# smooths.
#' 
#' @param model secr model specification list.
#' @param mask secr mask object.
#'   
#' @details (None as yet...)
#' 
#' @return Returns a \code{mask} object with updated covariates.
#' @export
#' @importFrom mgcv gam
#' @examples
#' data(Boland.leopards1)
#' head(Boland.mask1)
#' head(covariates(Boland.mask1))
#' newmask = prepare.mask.bases(~s(x, k = 3), Boland.mask1)
#' head(newmask)
#' head(covariates(newmask))

prepare.mask.bases = function(Dmodel, mask){
  
  # make the design matrix
  X = make.density.design.matrix(Dmodel, mask) # head(X)
  
  # add range of original covariates in Dmodel as an attribute
  # (will be added to fitted model for help with plotting)
  covs = attr(X, "term.labels") # names of covariates in Dmodel
  attr(mask, "cov.range") = apply(cbind(mask, covariates(mask))[, covs, drop = FALSE], 2, range)
  
  # replace the mask covariates with the design matrix
  covariates(mask) = as.data.frame(X)
  
  return(mask)   
  
}
