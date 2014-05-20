#' @title Returns covariate values of mask point closest to each trap.
#'
#' @description
#' Finds mask point closest to each trap then assigns to the trap the covariate value in 
#' \code{attributes(mask)$covariates[[cname]]} that is associated with this mask point.
#'  
#' @param traps Trap object.
#' @param mask Mask object.
#' @param cname Name of the covariate associated with the mask that you want to attach to the traps.
#' 
#' @details
#' Finds mask point closest to each trap then assigns to the trap the covariate value in 
#' attributes(mask)$covariates[[cname]] that is associated with this mask point.
#' 
#' @return
#' Returns a vector with covariate values associated with each trap, in the same order as they appear 
#' in \code{traps}
#' @export
trap.covar=function(traps,mask,cname){
  if(!is.element(cname,names(attr(mask,"covariates")))) stop(paste("Variable ",cname," is not in mask covariates",sep=""))
  mask.xy=data.frame(x=mask$x,y=mask$y)
  trap.xy=data.frame(x=traps$x,y=traps$y)
  trapstart=dim(mask.xy)[1]+1
  xy=rbind(mask.xy,trap.xy)
  trapend=dim(xy)[1]
  dists=as.matrix(dist(xy))[trapstart:trapend,1:(trapstart-1)] # distances from traps (rows) to mask points (columns)
  mask.z=attributes(mask)$covariates[[cname]]
  ntraps=length(trap.xy$x)
  trap.z=rep(NA,ntraps)
  for(i in 1:ntraps){
    trap.z[i]=mask.z[min(which(dists[i,]==min(dists[i,])))] # covariate value at closest mask point
  }
  return(trap.z)
}
