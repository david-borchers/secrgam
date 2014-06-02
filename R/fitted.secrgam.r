
#' @title Extract fitted density values from an secrgam model
#' 
#' @description description...
#'   
#' @param fit a fitted \code{\link{secrgam}} or \code{\link{secr}} model
#' @param mask a \code{\link{mask}} object
#' @details details...
#' @return value...
#' @examples
#' # construct a tensor product model to represent a bivariate normal
#' data(Boland.leopards)
#' fitted(Boland.fit)
#' @export

fitted.secrgam = function(fit, mask = NULL){
  
  if(fit$CL) stop("Fitted model used conditional likelihood, therefore no density model")
  
  if(is.null(fit$fixed$D)){
    
    # density parameter not fixed
    Dmodel = formula(paste("D", paste(fit$model$D, collapse = ""), collapse = ""))
    Dpars = fit$fit$estimate[fit$parindx$D]
    
  }else{
    
    # density parameter fixed
    Dmodel = D ~ 1 
    Dpars = fit$fixed$D 
    
  }
  
  # use mask from fitted model if mask not supplied separately
  if(is.null(mask)){
    
    mask = fit$mask 
    
  }else{
    
    if(!inherits(mask, "mask")) stop("'mask' must be a mask object")
    
  }
  
  # make dummy data for use in gam
  data = cbind(D = 1, mask, attributes(mask)$covariates)
  
  # use gam to get the design matrix
  X = gam(Dmodel, gaussian(link = fit$link$D), data, fx = TRUE, fit = FALSE)$X
  
  # evaluate the model for each mask point
  as.numeric(switch(
    fit$link$D, 
    "identity" = X %*% Dpars,
    "log" = exp(X %*% Dpars)
  ))
  
}