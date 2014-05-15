#' @title SECR Density B-pline plotting.
#'
#' @description
#'  Plots B-spline smooth components of an secr density model
#'  
#' @param fit object returned by secr.fit().
#' @param type either "link" (for plot on link funtion scale) or "response" (for plot on
#' response scale).
#' @param mask.rug TRUE if a rugplot of locations of mask points in the covariate dimension
#' is to be plotted.
#' @param det.rug TRUE if a rugplot of locations of detectors in the covariate dimension
#' is to be plotted.
#' @param npts number of points on x-axis at which to evaluate smooth.
#' @param show.knots if TRUE, puts crosses on smooth at knot locations.
#' 
#' @details
#' (None as yet)
#' @export
plotDgam=function(fit,type="link",mask.rug=FALSE,det.rug=TRUE,npts=200,show.knots=TRUE){
  if(type!="link" & type!="response") {
    type="link"
    warning("Invalid type; rest to to `link'.")
  }
  bases=attributes(fit$mask)$bases
  if(fit$model$D=="~1") stop("Density model has no variables so can't plot anything.")
  nsp=length(bases)
  # initialise start and end positions of parameters for current basis
  start=1+1;end=1+1 # +1 is for intercept
  for(i in 1:nsp) {
    D.df=dim(bases[[i]])[[2]]
    end=start+D.df-1 # update end point
    if(D.df>1) { # if D.df==1 then is not a smoothed variable
      bsvar=strsplit(strsplit(colnames(bases[[i]])[1],"_",fixed=TRUE)[[1]][2],"_",fixed=TRUE)[[1]][1]
#      bsvar=strsplit(colnames(bases[[i]])[1],".",fixed=TRUE)[[1]][2]
      main=paste("s(",bsvar,",",D.df,")",sep="")
      knots=attributes(bases[[i]])$Boundary.knots
      x=seq(knots[1],knots[2],length=npts)
      D.degree=attributes(bases[[i]])$degree
      inknots=attributes(bases[[i]])$knots
      if(length(inknots)>0) knots=sort(c(knots,inknots))
      X=bs(c(x,knots),df=D.df,degree=D.degree,intercept=FALSE)
      D.beta=fit$fit$estimate[start:end] # get relevant parameters
      # smooth (without intercept)
      if(type=="response") smooth.x=exp(as.numeric(X%*%D.beta))
      else smooth.x=as.numeric(X%*%D.beta)
      meansm=mean(smooth.x[1:npts])
      plot(x,smooth.x[1:npts]-meansm,type="l",ylim=range(smooth.x-mean(smooth.x)),xlab=bsvar,ylab=main,main=main)
      if(show.knots) points(knots,smooth.x[npts+1:length(knots)]-meansm,pch="+")
    } else { # here for linear effect
      bsvar=names(bases)[i]
      main=paste(bsvar,"*","beta_",bsvar,sep="")
      varvals=bases[[i]] # covariate values in mask
      x=seq(min(varvals),max(varvals),length=npts)
      D.beta=fit$fit$estimate[start:end] # get relevant parameters
      # smooth (without intercept)
      if(type=="response") smooth.x=exp(as.numeric(x*D.beta))
      else smooth.x=as.numeric(x*D.beta)
      meansm=mean(smooth.x[1:npts])
      plot(x,smooth.x[1:npts]-meansm,type="l",ylim=range(smooth.x-meansm),xlab=bsvar,ylab=main,main=main)
    }
    if(mask.rug | det.rug){
      # get observed variable values on mask:
      if(bsvar=="x" | bsvar=="y") {
        zvals=fit$mask[[bsvar]]
        det.zvals=traps(fit$capthist)[[bsvar]]
      } else {
        zvals=attr(fit$mask,"covariates")[[bsvar]]
        det.zvals=trap.covar(traps(fit$capthist),fit$mask,bsvar)
      }
      # then add rug:
      if(mask.rug)
        points(zvals,rep(min(smooth.x-meansm),length(zvals)),pch="|",cex=0.5,col="gray")
      if(det.rug)
        points(det.zvals,rep(min(smooth.x-meansm),length(det.zvals)),pch="|",cex=0.75)
    }
    start=start+D.df # update start point for next variable
  }
}