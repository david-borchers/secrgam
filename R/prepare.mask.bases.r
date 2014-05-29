
#' @title Add spline basis functions to mask covariates.
#'   
#' @description Replaces attr(mask,"covariates") with a data frame containing
#' new variables comprising basis function values for smoopthed variables,
#' toghether with non-smoothed variables (but excluding "x" and "y", which are
#' in mask already). 
#' 
#' Also adds a list of basis functions to
#' attributes(mask)$bases, and the df of each basis in
#' attributes(mask)$bases.df. The list attribtes(mask)$bases contains original
#' variables as 1D matrix if the variable is not smoothed, and spline basis
#' object (a >1D matrix with attributes givning knots, etc.) if it is smoothed. 
#' The vector attr(mask,"bases.df") contains the degrees of freedom of the
#' smooths.
#' 
#' @param model secr model specification list.
#' @param mask secr mask object.
#'   
#' @details (None as yet...)
#' 
#' @return Returns NULL if the Density model in model has no smooth terms (of
#'   form "s(x,k=b)"), else returns mask object with attributes "bases" and
#'   "covariates" as described above.
#' @export
#' @importFrom mgcv gam

prepare.mask.bases = function(model, mask){
  
  if(!"D" %in% names(model)) 
    stop("No D model in model object.")
  
  # make a dummy data frame with a response column
  mask$D = 1
  # put x and y in mask
  covariates(mask)=cbind(covariates(mask),x=mask$x)
  covariates(mask)=cbind(covariates(mask),y=mask$y)
  
  # add mask attributes (if there are any)
  if(!is.null(attr(mask, "covariates"))) 
    for(cov in names(attr(mask, "covariates")))
      mask[[cov]] = attr(mask, "covariates")[[cov]]

  # update the density formula so it has a left hand side
  form = update.formula(model$D, D ~ .)
  
  # use gam to get the design matrix
  G = gam(formula = form, data = mask, fit = FALSE)
  
  # extract the design matrix  
  X = G$X 
  
  # add the column names
  colnames(X) = G$term.names 
  
  # clean up the names so they can be used inside formula objects (without the use of backticks)
  colnames(X) = gsub("\\(", ".", colnames(X)) 
  colnames(X) = gsub("[\\)]|[,]", "", colnames(X)) 
  
  # set up a few things to help plotting later:
  covs=attr(G$terms,"term.labels") # names of covariates in model
  covrange=apply(covariates(mask)[,covs,drop=FALSE],2,range) # range of covariates in model
  
  # replace the mask attributes with the new design matrix
  attributes(mask)$covariates = as.data.frame(X)
  
  return(list(mask=mask,cov.range=covrange))   
  
}
