
#' @title Construct a design matrix from a density model
#'   
#' @description description...
#' 
#' @param model secr model specification list.
#' @param mask secr mask object.
#'   
#' @details details...
#' 
#' @return return...
#' @export
#' @importFrom mgcv gam

make.density.design.matrix = function(Dmodel, mask){
  
  # combine x and y mask coordinates with mask attributes (if there are any)
  if(!is.null(attr(mask, "covariates"))) 
    for(cov in names(attr(mask, "covariates")))
      mask[[cov]] = attr(mask, "covariates")[[cov]]
  
  # update the density formula so it has a left hand side (otherwise gam will throw an error)
  Dmodel = update.formula(Dmodel, D ~ .)
  
  # add a dummy D column 
  mask$D = 1
  
  # use gam to get the design matrix (but don't fit the model)
  G = gam(formula = Dmodel, data = mask, fit = FALSE)
  
  # extract the design matrix  
  X = G$X 
  
  # add the column names
  colnames(X) = G$term.names 
  
  # clean up the names so they can be used inside formula objects (without the use of backticks)
  colnames(X) = gsub("\\(", ".", colnames(X)) 
  colnames(X) = gsub("[\\)]|[,]", "", colnames(X)) 
  
  # names of original covariates
  attr(X, "term.labels") = attr(G$terms, "term.labels")
  
  return(X)   
  
}
