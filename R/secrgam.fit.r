
#' @title SECR fit with B-pline Density GAM.
#'   
#' @description Fits GAM using regression B-splines for density model. Does this
#'   by creating B-spline basis functions (which are stored in mask object) and 
#'   then calling function \code{\link{secr.fit}} using these.
#'   
#' @param capthist capture history, as for function \code{secr.fit}.
#' @param model model list, as for function \code{secr.fit}, but allowing terms
#'   of the form \code{s(var,k=df)}, where var is a mask coordinate (x or y) or
#'   a variable in \code{attr(mask, "covariate")} and df is the degrees of
#'   freedom of the B-spline smooth (i.e. the number of knots).
#' @param mask, as for function \code{secr.fit}
#' @param ... other arguments to \code{secr.fit}
#'   
#' @details Returns an object of class "secr", i.e., the same class of object as
#'   is returned by \code{secr.fit} of package \code{\link{secr}}.
#' @export

secrgam.fit = function(capthist, model, mask, ...){
  

  # make sure model is a named list
  model = secr:::stdform(model)
  orig.model=model # save model in original form 
  orig.mask=mask # save mask in original form
  
  # make a new mask that includes the design matrix for the smooth within the covariates attribute
  gamprep = prepare.mask.bases(model, mask)
  gamask =gamprep$mask
  
  # get the design matrix term names 
  Dparnames = colnames(attr(gamask, "covariates")) 
  
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
  
  # add basis covariates to mask
  covariates(mask)=covariates(gamask)
  
  # fit with secr.fit
  fit = secr.fit(capthist = capthist, model = model, mask = mask, ...)
  
  # add original model and orginal mask to fit object
  fit$Dmodel=orig.model$D
  fit$orig.mask=orig.mask
  
  # add stuff to use in plotting:
  fit$cov.range=gamprep$cov.range
  
  class(fit) = c("secrgam", class(fit))
  
  return(fit)
  
}

