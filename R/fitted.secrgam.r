
#' @title Get fitted density values from an secrgam model
#' 
#' @description description...
#'   
#' @param fit a fitted \code{\link{secrgam}} or \code{\link{secr}} model
#' @param mask a \code{\link{mask}} object
#' @details details...
#' @return value...
#' @examples
#' data(Boland.leopards)
#' fitted(Boland.fit)
#' @export
#' @seealso plot.secrgam

fitted.secrgam = function(fit, mask = NULL){
  
  if(fit$CL) stop("Fitted model used conditional likelihood, therefore no density model")
  
  # the Dmodel element gives the original D model passed to secrgam.fit - e.g. in terms of s() or te()
  # (whereas the D element of the model list gives the interpreted regression model passed to secr.fit)
  
  # is D model fixed?
  fixedD = is.null(fit$model$D) ; fixedD
    
  # get D parameters
  Dpars = if(fixedD) fit$fixed$D else fit$fit$estimate[fit$parindx$D] ; Dpars
  Dparnames = if(fixedD) "D" else fit$betanames[fit$parindx$D] ; Dparnames

  # use the mask from fit if an alternative mask is not supplied
  X = if(is.null(mask)){
    
    as.matrix(covariates(fit$mask))
    
  }else{
    
    if(!inherits(mask, "mask")) stop("'mask' must be a mask object")
    
    Dmodel = if(fixedD)  ~ 1 else fit$Dmodel
      
    make.density.design.matrix(Dmodel, mask) 
    
  } # head(X)
  
  # evaluate the model for each mask point
  as.numeric(switch(
    fit$link$D, 
    "identity" = X %*% Dpars,
    "log" = exp(X %*% Dpars)
  ))
  
}

