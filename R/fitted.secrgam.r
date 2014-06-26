
#' @title Get fitted density values from an secrgam model
#' 
#' @description Get fitted density values for each point on the mask or average density across the mask. The delta method is used to calculate standard errors for fitted values.
#'   
#' @param fit a fitted \code{\link{secrgam}} or \code{\link{secr}} model
#' @param mask a \code{\link{mask}} object
#' @param type the type of fitted value required: use \code{type = "individual"} for fitted values at each individual mask point, and \code{type = "mean"} for the estmated mean density across all mask points.
#' @param se if \code{TRUE}, upper and lower confidence intervals are returned 
#' @param level confidence interval level 
#' @details details...
#' @return If \code{se = FALSE} a numeric vector of fitted values is returned (or a single value if \code{type = "mean"}. If \code{se = TRUE} a list is returned with elements \code{lower}, \code{estimate} and \code{upper}.
#' @examples
#' data(Boland.leopards1)
#' data(Boland.fits1)
#' op = par(no.readonly = TRUE)
#' 
#' # fitted density values at each mask point
#' Dhat = fitted(fit1.a3)
#' par(mfrow = c(1,1))
#' prep4image(list(x = fit1.a3$mask$x,
#'                 y = fit1.a3$mask$y,
#'                 z = Dhat), asp = 1)
#' 
#' # fitted density values at each mask point plus upper and lower 95% CIs
#' Dhat = fitted(fit1.a3, se = TRUE)
#' par(mfrow = c(1,3))
#' for(i in names(Dhat))
#'   prep4image(list(x = fit1.a3$mask$x,
#'                   y = fit1.a3$mask$y,
#'                   z = Dhat[[i]]), main = i, asp = 1, zlim = range(Dhat))
#' par(op)
#' 
#' # fitted mean density across all mask points
#' Dhat = fitted(fit1.a3, type = "mean") ; Dhat
#' M = nrow(fit1.a3$mask) ; M # number of mask points
#' a = attr(fit1.a3$mask, "a") ; a # area of each mask cell
#' A = M * a ; A # total area of mask
#' Nhat = Dhat * A ; Nhat # abundance within mask
#' 
#' # fitted mean density across all mask points plus upper and lower 95% CI
#' Dhat = fitted(fit1.a3, se = TRUE, type = "mean") ; Dhat
#' Nhat = lapply(Dhat, function(x) x * A) ; Nhat
#' @export
#' @seealso \link{plot.secrgam}

fitted.secrgam = function(fit, mask = NULL, se = FALSE, type = c("individual","mean"), level = 0.95){
  
  if(fit$CL) stop("Fitted model used conditional likelihood, therefore no density model")
  
  type = match.arg(type)
  
  # need error check for level
  
  # the Dmodel element gives the original D model passed to secrgam.fit - e.g. in terms of s() or te()
  # (whereas the D element of the model list gives the interpreted regression model passed to secr.fit)
  
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
    
    a = attr(mask, "area")
    
  } # head(X)
  
  M = nrow(X) 
  colnames(X) = names(Dbeta) = Dbetanames
    
  # ----------------------------------------------------------------------------
  # fitted values 
  
  Dhat = X %*% Dbeta
  
  # check hessian
  if(se){
    
    if(is.null(fit$fit$hessian)){
      
      warning("confidence intervals not calculated (hessian is missing)")
      
    }else{
      
      Cov = solve(fit$fit$hessian[fit$parindx$D,fit$parindx$D])
      
      if(!all(is.finite(Cov))){
        
        warning("confidence intervals not calculated (hessian contains non-finite values)")
        
      }
      
    }
    
  }
    
  if(type == "individual"){
    
    Dhat = as.numeric(Dhat)
    
    if(se){
      
      #           seDhat = sapply(1:nrow(X), function(i){ # i=1
      #             
      #             a_j.a_k = sapply(1:ncol(X), function(j){ 
      #               
      #               sapply(1:ncol(X), function(k){
      #                 
      #                 X[i,j] * X[i,k]
      #                 
      #               })
      #               
      #             })
      #             
      #             sqrt(sum(a_j.a_k * Cov))
      #             
      #           })
      
      seDhat = sqrt(delta.method(X, Dbeta, Cov, link = "identity"))
      
      Dhat = list(
        lower    = Dhat - 1.96 * seDhat,
        estimate = Dhat,
        upper    = Dhat + 1.96 * seDhat
      )
      
      if(fit$link$D == "log") Dhat = lapply(Dhat, exp)      
      
    }else{
      
      if(fit$link$D == "log") Dhat = exp(Dhat)
      
    }
    
  }else{
    
    Dhat = if(fit$link$D == "log") sum(exp(Dhat)) / M else sum(Dhat) / M ; Dhat

    if(se){
      
      seDhat = sqrt(delta.method(X, Dbeta, Cov, type = "mean", link = fit$link$D)) ; seDhat
      
      Nhat = Dhat * M * a ; Nhat
      
      cvDhat = seDhat / Dhat ; cvDhat
      
      Dhat = lognormal.CI.Nhat(n = nrow(fit$capthist), Nhat, cv = cvDhat) ; Dhat
      
      Dhat = lapply(Dhat, function(x) x / (M * a)) ; Dhat
      
    }
    
  }
  
  return(Dhat)
  
}

