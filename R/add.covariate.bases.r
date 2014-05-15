#' @title Add basis functions to attr(mask,"covariates").
#'
#' @description
#'  Adds all basis functions to attr(mask,"covariates"), including linear (non-smoothed)
#'  explanatory variables, as a single data frame. (This data frame is not currently used
#'  but seems necessary for use with secr.fit.)
#'  
#' @param mask secr mask object.
#' @export
add.covariate.bases=function(mask){
  bases=attr(mask,"bases")
  nbases=length(bases)
#  cov=bases[[1]]
#  if(nbases>1) for(i in 2:nbases) cov=cbind(cov,bases[[i]])
#  attr(mask,"covariates")=as.data.frame(cov)
  cnames=names(attr(mask,"covariates"))
  for(i in 1:nbases) {
    bname1=colnames(bases[[i]])[1]
    got=((dim(bases[[i]])[2]==1 & is.element(bname1,cnames)) | bname1=="x" | bname1=="y")
    if(!got) attr(mask,"covariates")=cbind(attr(mask,"covariates"),bases[[i]])
  }
  return(mask)
}