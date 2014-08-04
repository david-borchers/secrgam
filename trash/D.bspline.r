#' @title B-spline density surface evaluation.
#'
#' @description
#' Calculates density at all points in mask, using basis functions in attr(mask,"bases")
#' and parameter values in Dbeta. Currently only works for 1 session and with ngroup=1.
#' @param Dbeta model parameter values.
#' @param mask secr mask object as output by prepare.mask.bases, i.e. with attr(mask,"bases")
#' added as appropriate.
#' @param ngroup see help for \code{userDfun} in package \code{\link{secr}}.
#' @param nsession number of sessions - see help for \code{userDfun} in package 
#' \code{\link{secr}}.
#' 
#' @details
#' This function conforms to the specifiction for user-defined functions given in the
#' vignette "modelling density surfaces" of package "secr". It is intended for use with 
#' the function secr.fit() via the userDfn component of the details argument, as follows:
#' secr.fit(...,details = list(userDfn = userDfn0),...).
#' @export
D.bspline=function(Dbeta, mask, ngroup=1, nsession=1){
  bases=attributes(mask)$bases
  nsp=length(bases)
  if(Dbeta[1]=="name") {
    # Reconstruct model from names in attributes(mask)$bases
    first=TRUE
    for(i in 1:nsp) {
      df=attr(mask,"bases.df")
      bnames=dimnames(bases[[i]])[[2]]
      if(df[i]==1) {
        if(first) {
          Mname=bnames
          first=FALSE
        } else {
          Mname=paste(Mname,"+",bnames)
        }
      } else {
        bsvar=strsplit(strsplit(bnames[df[i]],"_",fixed=TRUE)[[1]][2],"_",fixed=TRUE)[[1]][1]
        sname=paste("s(",bsvar,",k=",df[i],")",sep="")
        if(first) {
          Mname=sname
          first=FALSE
        } else {
          Mname=paste(Mname,"+",sname)
        }
      }
    }
    return(Mname)
  }
  if(Dbeta[1]=="parameters") {
    # extract all variable names (including basis variable names)
    pnames="Intercept"
    for(i in 1:nsp) pnames=c(pnames,colnames(bases[[i]]))
    return(pnames)
  }
  # here to calculate density
  D.df=rep(NA,nsp)
  for(i in 1:nsp) D.df[i]=dim(bases[[i]])[[2]]
  lp=Dbeta[1]
  end=1+cumsum(D.df)
  start=c(2,end[-nsp]+1)
  for(i in 1:nsp) lp=lp + bases[[i]]%*%Dbeta[start[i]:end[i]]
  D <- exp(lp)
  tempD=array(D,dim=c(nrow(mask),ngroup,nsession)) 
  return(tempD)  
}