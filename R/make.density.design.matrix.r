
#' @title Construct a design matrix from a density model
#'   
#' @description Helper function to create a design matrix from a conventional density formula (e.g. including \code{\link{s}} and/or \code{\link{te}} terms) and a mask object (i.e. a dataframe of covariates). 
#' 
#' @param model density model
#' @param mask secr mask object (or list of mask objects).
#'   
#' @details Uses the \code{\link{gam}} function from the \code{\link{mgcv}} package to generate the design matrix (by setting \code{fix = FALSE}).
#' @return Returns a matrix with number of rows equal to the number of rows in \code{mask}, and number of columns equal to the number of parameters required to implement the regresssion spline specified in \code{model}.
#' @export
#' @importFrom mgcv gam
#' @examples
#' data(Boland.leopards1)
#' X = make.density.design.matrix(~s(x, k = 3), Boland.mask1)
#' head(X)

make.density.design.matrix = function(Dmodel, mask, nsessions = 1){
  
  # convert mask to a list (if it's not already a list)
  if(!inherits(mask, "list")) mask = list(mask)
  
  if(length(mask) != nsessions)
    stop("if mask supplied as a list, then length(mask) must equal nsessions")         
  
  # index for mask-session
  sessionid = rep(1:nsessions, lapply(mask, nrow)) 
  
  # add a dummy 'D' column 
  # combine mask with mask covariates (if there are any)
  # combine individual session masks into single mask
  tempdata = do.call(rbind, lapply(mask, function(x){ # x = mask[[1]]
    x$D = 1
    if(is.null(covariates(x))) x else cbind(x, covariates(x))
  }))
  
  # update the density formula so it has a left hand side (otherwise gam will throw an error)
  Dmodel = update.formula(Dmodel, D ~ .)
  
  # use gam to get the design matrix (but don't fit the model)
  G = gam(formula = Dmodel, data = tempdata, fit = FALSE)
  
  # extract the design matrix  
  X = G$X 
  
  # add the column names
  colnames(X) = G$term.names 
  
  # clean up the names so they can be used inside formula objects (without the use of backticks)
  colnames(X) = gsub("\\(", ".", colnames(X)) 
  colnames(X) = gsub("[\\)]|[,]", "", colnames(X)) 
  
  ## split X by session and return as a list
  # X = lapply(1:nsessions, function(i) X[sessionid == sessionid[i],])
  
  # add session id as attribute
  attr(X, "session.id") = sessionid 
  
  # add names of original covariates as an attribute
  temp = rownames(attributes(G$terms)$factors)
  temp = temp[temp != "D"]
  attr(X, "term.labels") = temp 
  
  return(X)   
  
}


