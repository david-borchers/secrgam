#' @title Abundance and Density in a region.
#'   
#' @description Estimate the expected and realised populations in a region, using a 
#' fitted spatially explicit capture-recapture model. Density is assumed to follow 
#' an inhomogeneous Poisson process in two dimensions. Expected N is the volume 
#' under a fitted density surface; realised N is the number of individuals within 
#' the region for the current realisation of the process (cf Johnson et al. 2010; 
#' see Note). 
#' 
#' This function is an adapted version of \code{region.N} of package \code{secr}, to
#' report density as well as abunance, and to accommodate regression spline models 
#' for density.
#'
#' @param object secr object output from secr.fit
#' @param region mask object defining the possibly non-contiguous region for which 
#' population size is required, or vector polygon(s) (see Details)
#' @param spacing	spacing between grid points (metres) if region mask is 
#' constructed on the fly. This argument must be NULL if object is of class \code{sercgam}
#' @param session	character session
#' @param group	group for future use
#' @param se.N logical for whether to estimate SE(N-hat) and confidence interval
#' @param alpha	alpha level for confidence intervals
#' @param loginterval	logical for whether to base interval on log(N)
#' @param nlowerbound	logical for whether to use n as lower bound when computing 
#' log interval for realised N
#' @param RN.method	character string for method used to calculate realised N (RN) 
#' and its sampling variance. 'poisson' or 'MSPE'.
#' 
#' @details If the density surface of the fitted model is flat (i.e. object$model$D == ~1 or 
#' object$CL == TRUE) then E(N) is simply the density multiplied by the area of 
#' region, and the standard error is also a simple product. In the conditional 
#' likelihood case (not valid for \code{secrgam} objects), the density and 
#' standard error are obtained by first calling derived.
#' 
#' If, on the other hand, the density has been modelled then the density surface is 
#' predicted at each point in region and E(N) is obtained by discrete summation. 
#' Pixel size may have a minor effect on the result - check by varying spacing. 
#' Sampling variance is determined by the delta method, using a numerical 
#' approximation to the gradient of E(N) with respect to each beta parameter.
#' 
#' The region may be defined as a mask object (if omitted, the mask component of 
#' object will be used). Alternatively, region may be a SpatialPolygonsDataFrame 
#' object (see package sp), and a raster mask will be constructed on the fly using 
#' the specified spacing. See make.mask for an example importing a shapefile to a 
#' SpatialPolygonsDataFrame. 
#' 
#' Note: The option of specifying a polygon rather than a mask for region does not 
#' work if the density model in object uses spatial covariates: these must be 
#' passed in a mask. 
#' 
#' Group-specific N has yet to be implemented. 
#' 
#' Population size is adjusted automatically for the number of clusters in 
#' 'mashed' models (see mash). However, the population size reported is that 
#' associated with a single cluster unless regionmask is specified.
#' @return
#' A list with components \code{$Abundance} and \code{$Density}. If se.N = FALSE, 
#' these are the numeric values of expected population size and density, otherwise 
#' they each contain a dataframe with rows 'E.N' and 'R.N' for \code{$Abundance}, 
#' 'E.D' and 'R.D' for \code{$Abundance}, with columns as below. 
#' 
#' estimate   estimate of N (expected or realised, depending on row)
#' SE.estimate	 standard error of estimated N
#' lcl	 lower 100(1--alpha)% confidence limit
#' ucl	 upper 100(1--alpha)% confidence limit
#' n	 total number of individuals detected
#' 
#' For multiple sessions, a list with one component per session is returned, 
#' each component as above.
#' 
#' @export
#' @examples
#' \dontrun{
#' data(Boland.fits1) # get model fitted to Boland leopard data
#' fit1.a3.dW3 # look at a fitted model
#' 
#' # plot fitted surface:
#' plot(fit1.a3.dW3, asp=1)
#' plot(traps(fit1.a3.dW3$capthist), add = TRUE)
#' 
#' # calculate abundance and density estimates on fitting mask:
#' region.ND(fit1.a3.dW3)
#' 
#' data(Boland.leopards2) # get larger mask
#' # calculate abundance and density estimates on fitting mask:
#' region.ND(fit1.a3.dW3,Boland.mask2)
#' }
region.ND=function (object, region = NULL, spacing = NULL, session = NULL, 
          group = NULL, se.N = TRUE, alpha = 0.05, loginterval = TRUE, 
          nlowerbound = TRUE, RN.method = "poisson") 
{
  if(inherits(object,"secrgam") & !is.null(region)){
    
    # stop with error if required to create mask
    if(!is.null(spacing)) stop("Argument 'spacing' must be NULL with secrgam object: can't create mask with covariates automatically.")
    
    Dmodel=object$Dmodel
    if(!is.null(Dmodel)) region = prepare.mask.bases(Dmodel, region)
    
    # get the design matrix term names 
    Dparnames = colnames(covariates(region)) 
    
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
    object$model$D = formula(form)
    
  }
  # Abundance estimate:
  regN=region.N(object, region=region, spacing=spacing, session=session, 
           group=group, se.N=se.N, alpha=alpha, loginterval=loginterval, 
           keep.region=FALSE, nlowerbound=nlowerbound, RN.method=RN.method)
  # Calculate total area of region:
  if(is.null(region)) region=object$mask
  if(ms(region)){ # multi-session region
    outp=region # set up list of correct length
    ns=length(outp)
    for(i in 1:ns){
      A=nrow(region[[i]])*attr(region[[i]],"a")
      # Density estimate
      regD=regN[[i]]/A
      if(se.N) {
        rownames(regD)=c("E.D","R.D")
        regD$n=regN[[i]]$n
      }
      outp[[i]]=list(Abundance=regN[[i]],Density=regD)
    }
  } else {
    A=nrow(region)*attr(region,"a")
    # Density estimate
    regD=regN/A
    if(se.N) {
      rownames(regD)=c("E.D","R.D")
      regD$n=regN$n
    }
    outp=list(Abundance=regN,Density=regD)
  }
  class(outp)="list"
  attributes(outp)=NULL
  return(outp)
}

