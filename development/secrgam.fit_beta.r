
#' @title SECR fit with flexible Density models.
#'   
#' @description Fits GAM using regression splines for the density model. Does this
#'   by creating spline basis functions (which are stored as covariates in a mask object) 
#'   and then calling function \code{\link{secr.fit}} to operate on these.
#'   
#' @param capthist capture history, as for function \code{secr.fit}.
#' @param model model list, as for function \code{secr.fit}, but allowing terms
#'   of the form \code{s(var,k=df)},  \code{s(var,...,k=df)},  \code{te(var,...,k=df)}
#'   where var is a mask coordinate (x or y) or a variable in \code{attr(mask, "covariate")}, 
#'   ``...'' is a comma-separated list of other such variables, and df is the degrees 
#'   of freedom of the spline basis (the number of knots).
#' @param mask, as for function \code{secr.fit}
#' @param ... other arguments to \code{secr.fit}
#'   
#' @details Returns an object of class c("secrgam","secr"), the latter being same class of 
#'   object as is returned by \code{secr.fit} of package \code{\link{secr}}.
#' @export

secrgam.fit = function(capthist, model, mask, ...){
  
  # need to deal with fixed density...
  
  # make sure model is a named list
  model = secr:::stdform(model)
  orig.model = model # save model in original form 
  orig.mask = mask # save mask in original form
  
  if(!is.null(model$D)) mask = prepare.mask.bases(model$D, mask) # head(mask) ; head(covariates(mask)) ; attr(mask, "cov.range")
  
  # get the design matrix term names 
  # Dparnames = colnames(attr(gamask, "covariates")) 
  Dparnames = colnames(covariates(mask)) 
  
  # identify whether or not there is an explicit intercept term
  int = grepl("Intercept", Dparnames) 
  
  # if so, delete this from the term names
  Dparnames = Dparnames[!int] 
  
  # make a new formula for density
  # if no intercept then use -1
  form = if(any(int)) "~1" else "~-1"
  
  # if any non-intercept par names then add them to the formula
  if(length(Dparnames) > 0)
    form = paste(form, paste(Dparnames, collapse = " + "), sep = " + ")
  
  # replace density formula
  model$D = formula(form)
  
  # fit with secr.fit
  fit = secr.fit(capthist = capthist, model = model, mask = mask, ...)
  
  # add original model and orginal mask to fit object
  fit$Dmodel = orig.model$D
  fit$orig.mask = orig.mask
  
  # add stuff to use in plotting:
  #   fit$cov.range = gamprep$cov.range
  fit$cov.range = attr(mask, "cov.range")
  
  class(fit) = c("secrgam", class(fit))
  
  return(fit)
  
}

