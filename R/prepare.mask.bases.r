
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

prepare.mask.bases = function(Dmodel, mask, sessioncov = NULL, nsessions = 1){
  
  # convert mask to a list (if it's not already a list)
  if(!inherits(mask, "list")) mask = list(mask)
  
  if(nsessions > 1){
    
    if(length(mask) == 1){
      
      # single mask supplied, so make list of duplicate masks
      mask = lapply(1:nsessions, function(i) mask)
      
    }else{
      
      # mask supplied as list, so check length = nsessions
      if(length(mask) != nsessions)
        stop("if mask supplied as a list, then length(mask) must equal nsessions")         
      
    }
    
    # default session variables
    Session = (1:nsessions)-1 
    session = factor(Session) 
    
    if(is.null(sessioncov)){
      
      # no sessioncov supplied, so make default sessioncov data frame
      sessioncov = data.frame(Session, session) 
      
    }else{
      
      # sessionov supplied, so check nrows = nsessions
      if(nrow(sessioncov) != nsessions)
        stop("if sessioncov supplied as a list, then nrow(sessioncov) must equal nsessions")         
      
      # and add session and Session variables if not already present 
      if(is.null(sessioncov$Session)) sessioncov$Session = Session
      if(is.null(sessioncov$session)) sessioncov$session = session
      
    }
    
  }  
  
  # add session covariates to mask covariates
  for(i in 1:nsessions){ # i=1
    
    covariates(mask[[i]]) = if(is.null(covariates(mask[[i]]))){
      
      if(is.null(sessioncov)) NULL else{
        
        do.call(rbind, lapply(1:nrow(mask[[i]]), function(j) sessioncov[i,,drop = FALSE]))
        
      }
      
    }else{
      
      if(is.null(sessioncov)){
        
        covariates(mask[[i]])
        
      }else{
        
        cbind(covariates(mask[[i]]), sessioncov[i,])
        
      }
      
    }
    
  }
  
  # the design matrix
  X = make.density.design.matrix(Dmodel = Dmodel, mask = mask, nsessions = nsessions) # head(X)
  
  # add range of original covariates in Dmodel as an attribute
  # (will be added to fitted model for help with plotting)
  covs = attr(X, "term.labels") # names of covariates in Dmodel
  
  if(!is.null(covs)){
    #         temp = if(is.null(covariates(mask[[i]]))) mask[[i]] else
    #           cbind(mask[[i]], covariates(mask[[i]]))
    #         attr(mask, "cov.range") = apply(temp[, covs, drop = FALSE], 2, range)
    
    attr(mask, "cov.range") = do.call(cbind, lapply(covs, function(cov){
      
      range(do.call(c, lapply(mask, function(x){
        
        if(cov %in% c("x","y")) x[,cov] else covariates(x)[,cov]
        
      })))
      
    }))
    
    colnames(attr(mask, "cov.range")) = covs
    
  }
  
  # replace the mask covariates with the design matrices
  for(i in 1:nsessions) covariates(mask[[i]]) = as.data.frame(X[attr(X, "session.id") == i,,drop = FALSE])
  
  # add Dparnames attribute
  Dparnames = colnames(covariates(mask[[1]])) 
  
  if(nsessions == 1) mask = mask[[1]]
  
  attr(mask, "Dparnames") = Dparnames 

  return(mask)   
  
}
