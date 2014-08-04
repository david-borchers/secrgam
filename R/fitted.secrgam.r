
#' @title Get fitted density values from an secrgam model
#' 
#' @description Get fitted density values for each mak point or average density across the mask. 
#'   
#' @param fit a fitted \code{\link{secrgam}} or \code{\link{secr}} model
#' @param mask a \code{\link{mask}} object
#' @param type the type of fitted value required (use \code{type = "individual"} for fitted values at each individual mask point, or \code{type = "mean"} for the estmated mean density across all mask points).
#' @return If \code{se = FALSE} a numeric vector of fitted values is returned (or a single value if \code{type = "mean"}). If \code{se = TRUE} a list is returned with elements \code{lower}, \code{estimate} and \code{upper}.
#' @examples
#' data(Boland.leopards1)
#' data(Boland.fits1)
#' op = par(no.readonly = TRUE)
#' 
#' # fitted density values at each mask point
#' Dhat = fitted(fit1.a3)
#' par(mfrow = c(1,1))
#' prep4image(cbind(x = fit1.a3$mask$x,
#'                  y = fit1.a3$mask$y,
#'                  z = Dhat), asp = 1)
#' 
#' # fitted mean density across all mask points
#' Dhat = fitted(fit1.a3, type = "mean") ; Dhat
#' M = nrow(fit1.a3$mask) ; M # number of mask points
#' a = attr(fit1.a3$mask, "a") ; a # area of each mask cell
#' A = M * a ; A # total area of mask
#' Nhat = Dhat * A ; Nhat # abundance within mask
#' @export
#' @seealso \link{plot.secrgam}

fitted.secrgam = function(fit, mask = NULL, type = c("individual","mean")){
  
  if(fit$CL) stop("Fitted model used conditional likelihood, therefore no density model")
  
  type = match.arg(type)
  
  # is D model fixed?
  fixedD = is.null(fit$model$D) ; fixedD
  
  # get D parameters
  Dbeta = if(fixedD) fit$fixed$D else fit$fit$estimate[fit$parindx$D] ; Dbeta
  Dbetanames = if(fixedD) "D" else fit$betanames[fit$parindx$D] ; Dbetanames
  
  # use the mask from fit if an alternative mask is not supplied
  if(is.null(mask)){
    
    X = as.matrix(covariates(fit$mask))
    
    a = attr(fit$mask, "area")
    
  }else{
    
    if(!inherits(mask, "mask")) stop("'mask' must be a mask object")
    
    Dmodel = if(fixedD)  ~ 1 else fit$Dmodel
    
    X = make.density.design.matrix(Dmodel, mask) 
        
  } # head(X)
  
  M = nrow(X) 
  colnames(X) = names(Dbeta) = Dbetanames
    
  # ----------------------------------------------------------------------------
  # fitted values 
  
  Dhat = X %*% Dbeta
      
  if(type == "individual"){
    
    Dhat = as.numeric(Dhat)
    
    if(fit$link$D == "log") Dhat = exp(Dhat)
          
  }else{
    
    Dhat = if(fit$link$D == "log") sum(exp(Dhat)) / M else sum(Dhat) / M
    
  }
  
  return(Dhat)
  
}

