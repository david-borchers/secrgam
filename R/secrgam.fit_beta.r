
#' @title SECR fit with flexible Density models - beta version.
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
#' @examples
#' \dontrun{
#' data(Boland.leopards1)
#' model = list(D ~ s(alt, k = 4), g0 ~ 1, sigma ~ 1)
#' fit = secrgam.fit_beta(capthist = Boland.CH1, model = model, mask = Boland.mask1, trace = FALSE)
#' fit # look at fit results
#' 
#' # plot fitted surface:
#' plot(fit, asp=1)
#' plot(traps(Boland.CH1), add = TRUE)
#' 
#' # plot smooths:
#' plotDgam(fit)
#' }

secrgam.fit_beta = function(capthist, model = list(D ~ 1, g0 ~ 1, sigma ~ 1), mask = NULL, buffer = 100, sessioncov = NULL, ...){
  
  # multi-session data?
  nsessions = if(is.list(capthist)) length(capthist) else 1
  
  # check sessioncov
  if(!is.null(sessioncov))
    if(!is.data.frame(sessioncov))
      stop("please enter sessioncov as a dataframe")
  
  # make default mask if no mask supplied
  if(is.null(mask)){
    mask = make.mask(traps(capthist), buffer = buffer, type = "trapbuffer")
    message("using 'trapbuffer' mask with buffer = ", buffer)
  }
  
  # convert mask to a list (if not already a list)
  if(!inherits(mask, "list")) mask = list(mask)
  
  # make sure model is a named list
  if("formula" %in% class(model)) model = list(model)
  model = secr:::stdform(model)
  orig.model = model # save model in original form 
  orig.mask = mask # save mask in original form
  
  if(is.null(model$D)) model$D = ~1
  
  # replace mask covariates with gam model terms
  mask = prepare.mask.bases(Dmodel = model$D, mask = mask, sessioncov = sessioncov, nsessions = nsessions)
  
  # get the design matrix term names 
  # Dparnames = colnames(attr(gamask, "covariates")) 
  Dparnames = colnames(covariates(mask[[1]])) 
  
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

