#' @title Add B-splines to mask.
#'
#' @description
#'  Replaces attr(mask,"covariates") with a data frame containing new variables comprising 
#'  basis function values for smoopthed variables, toghether with non-smoothed variables 
#'  (but excluding "x" and "y", which are in mask already).
#'  Also adds a list of basis functions to attributes(mask)$bases, and the df of each
#'  basis in attributes(mask)$bases.df. The list attribtes(mask)$bases contains
#'  original variables as 1D matrix if the variable is not smoothed, and B-spline 
#'  basis object (a >1D matrix with attributes givning knots, etc.) if it is smoothed. 
#'  The vector attr(mask,"bases.df") contains the degrees of freedom of the smooths.
#'  
#' @param model secr model specification list.
#' @param mask secr mask object.
#' 
#' @details
#' (None as yet)
#' 
#' @return Returns NULL if the Density model in model has no smooth terms (of form "s(x,k=b)"), else 
#' returns mask object with attributes "bases" and "covariates" as described above.
prepare.mask.bases=function(model,mask){
  Dind=NULL
  for(i in 1:length(model)) {
    if(as.character(model[[i]])[2]=="D") Dind=i
  }
  if(is.null(Dind)) stop("No D model in model object.")
  gf=as.formula(model[[Dind]])
  #  p.env <- environment(gf)
  tf <- terms.formula(gf, specials = c("s"))
  terms <- attr(tf, "term.labels")
  nt <- length(terms)
  if (attr(tf, "response") > 0) {
    response <- as.character(attr(tf, "variables")[2])
    pf <- rf <- paste(response, "~", sep = "")
  } else pf <- rf <- "~"
  sp <- attr(tf, "specials")$s
  if(is.null(sp)) {
    return(sp)
  } else {
    off <- attr(tf, "offset")
    vtab <- attr(tf, "factors")
    if (length(sp) > 0) 
      for (i in 1:length(sp)) {
        ind <- (1:nt)[as.logical(vtab[sp[i], ])]
        sp[i] <- ind
      }
    nsp <- length(sp)
    svars=rep("",nsp)
    sdf=rep(NA,nsp)
    for(i in 1:nsp) {
      #    temp=eval(parse(text = terms[sp[i]]), envir = p.env)
      temp=eval(parse(text = terms[sp[i]]))
      svars[i]=temp$term
      sdf[i]=temp$bs.dim
    }
    if(length(terms)>nsp) vars=c(terms[-sp],svars)
    else vars=svars
    if(length(vars)>length(unique(vars))) stop("variables duplicated in Density model formula")
    df=rep(1,length(terms))
    df[sp]=sdf
    # create data frame
    X=mask
    if(!is.null(attributes(mask)$covariates)) X=cbind(mask,attributes(mask)$covariates)
    varnames=names(X)
    for(i in 1:length(vars)) if(!is.element(vars[i],varnames)) stop(paste("No data for variable",vars[i],"of Density model formula"))
    attributes(mask)$bases.df=df
    bases=vector("list", length=length(vars))
    names(bases)=vars
    for(i in 1:length(vars)){
      if(df[i]>1) {
        if(df[i]<3) warning(paste("Degree of B-spline reduced from 3 to ",df[i], " for variable ",vars[i],", because k=",df[i],".",sep=""))
        bases[[vars[i]]]<-bs(X[,vars[i]],df=df[i],degree=min(3,df[i]),intercept=FALSE) 
        dimnames(bases[[vars[i]]])[[2]]=paste("bs",1:df[i],"_",vars[i],"_",sep="")
      }else {
        bases[[vars[i]]]<-as.matrix(X[,vars[i]])
        colnames(bases[[vars[i]]])=vars[i]
      }
    }
    attributes(mask)$bases=bases
    mask=add.covariate.bases(mask) # make attr(mask,"covariates") a data frame comprising all bases
    return(mask)    
  }
}


#' @title Add basis functions to attr(mask,"covariates").
#'
#' @description
#'  Adds all basis functions to attr(mask,"covariates"), including linear (non-smoothed)
#'  explanatory variables, as a single data frame. (This data frame is not currently used
#'  but seems necessary for use with secr.fit.)
#'  
#' @param mask secr mask object.
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


#' @title SECR fit with B-pline Density GAM.
#'
#' @description
#'  Fits GAM using regression B-splines for density model. Does this by creating B-spline
#'  basis functions (which are stored in mask object) and then calling function secr()
#'  using these.
#'  
#' @param capthist capture history, as for function secr.fit().
#' @param model model list, as for function secr.fit(), but allowing terms of the form 
#' `s(var,k=df)', where var is a mask coordinate (x or y) or a variable in 
#' attr(mask,"covariate") and df is the degrees of freedom of the B-spline smooth 
#' (i.e. the number of knots).
#' @param mask, as for function secr.fit().
#' @param ... other arguments to secr.fit()
#' 
#' @details
#' Returns an object of class "secr", i.e., the same class of object as is returned by 
#' secr.fit of package \code{\link{secr}}.
secrgam.fit=function(capthist,model,mask,...){
  gamask=prepare.mask.bases(model,mask) # create basis functions in gamask
  if(is.null(gamask)) { # no smoothed terms
    Dfit=secr.fit(capthist=capthist,model=model,mask=mask,...)
  } else {
    Dparnames=D.bspline("parameters",gamask) # get basis function names
    Dmod=as.formula(paste("D~1+",paste(Dparnames[-1],collapse="+"),sep="")) # create formula with basis function names
    model$D=Dmod # replace Density model formula with that using basis functions
    Dfit=secr.fit(capthist=capthist,model=model,mask=gamask,...)
    # line below works OK, but falls down on print.secr() due to "s(x,k)" not being understood 
    # by secr.lpredictor, which is called by print.secr().
    #  Dfit$model$D=as.formula(paste("~",D.bspline("name",gamask))) # put model name in s(x,k) form
    # You can also fit using userDfn, but no real reason to do this, so next 3 lines commented out:
    #  if(is.null(details)) details=list(userDfn=D.bspline)
    #  else details$userDfn=D.bspline
    #  fit=secr.fit(capthist=capthist,model=model,mask=gamask,details=details,...) # call secr.fit using basis functions for D
  }
  return(Dfit)
}


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
#' 
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


#' @title Prepares data frame for plotting with image/contour/persp.
#'
#' @description
#'  From an input data frame with columns x, y and z, this function creates a list with elements
#'  x, y and z in format suitable for passing to functions image, contour or persp.
#'  
#' @param obj a data frame with columns x and y being Cartesian coordinates on a REGULAR RECTANGULAR GRID, 
#' and z being the values of some variable at each coordinate.
#' @param plot A logical indicating whether or not an image plot should be created (by way of checking the
#' output of this function).
#' @param ... other arguments to \code{\link{image}}; only used if plot==TRUE.
#' 
#' @details
#' Sorts z on values of x first, then y, then creates a matrix of z-values from this. Returns a list with
#' elemets x (unique values of x, in increasing order), y (unique values of y, in increasing order) and z
#' (matrix of z-values in appropriate order for image/contour/persp).
prep4image=function(obj,plot=FALSE,...){
  ord=order(obj$x,obj$y)
  x=sort(unique(obj$x))
  y=sort(unique(obj$y))
  z=matrix(obj$z[ord],nrow=length(x),byrow=TRUE)
  if(plot) image(x,y,z,...)
  return(list(x=x,y=y,z=z))
}



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
#' 
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

#---------------------------------- Datasets ------------------------------------
#' @name WCape.alt
#' @title Altitudes of terrain in Western Cape, South Africa.
#' @docType data
#' @description Altitudes of terrain in the Western Cape, South Africa.
#' @usage data(WCape.alt)
#' @format A data frame with the following elements $x, $y and $z, where $x and $y are longitude
#' and latitude and $z is altitude (in m).
#' @source www.ngdc.noaa.gov/mgg/global/
#' @references
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources 
#' and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
NULL

#' @name Boland.leopards
#' @title Data from 2010 camera-trap survey of leopards in the Boland, South 
#' Africa.
#' @docType data
#' @description The data are from a camera-trap survey of leopards in the Boland region of 
#' South Africa. Cameras were checked weekly and data recorded in binary format (i.e. only 
#' whether or not an animal was detected on each occasion, not the number of detections
#' within occasions). 
#' @usage data(Boland.leopards)
#' @format An object containing list with the following three data objects.
#'  \describe{
#'    \item{\code{Boland.CH}:}{ an \code{\link{secr}} capture history object with camera-trap 
#'    detections of leopards in the form of binary data over 13 occasions.}
#'    \item{\code{Boland.mask}:}{ an \code{\link{secr}} mask object for the data in \code{Boland.CH}.
#'    In \code{attr(Boland.mask,"covariates")} is a data frame with altitude data in column 
#'    \code{alt} and the product of longitude and altitude and of latitude and altitude in 
#'    columns \code{xalt} and \code{yalt}, both scaled by dividing by their mean.}
#'    \item{\code{Boland.image}:}{ altitude data for the \code{Boland.leopards} study region, 
#'    comprising a list in a format suitable for plotting with \code{\link{image}}. It has three 
#'    elements: $x, being a a vector of longitude values of a grid spanning the study area, 
#'    $y, being a a vector of latitude values of a grid spanning the study area, and $z, being 
#'    a matrix of altitudes, in format suitable for use with \code{\link{image}}.}
#'  }
#' @source \code{Boland.CH} and \code{Boland.mask} were provided by the Cape Leopard Trust 
#' Boland Leopard Project (http://capeleopard.org.za/research/leopard/boland). 
#' \code{Boland.image} was downloaded from www.ngdc.noaa.gov/mgg/global/ (and then formatted 
#' using \code{\link{prep4image}}).
#' @references
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources 
#' and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
NULL

#' @name secr.possum
#' @title Data from a trapping study of brushtail possums at Waitarere, North Island, 
#' New Zealand. 
#' @docType data
#' @description These data are taken directly from package \code{secr}. Type 
#' \code{help(possum)} for a detailed description of the data.
#' @usage data(secr.possum)
#' @format An object containing the following data objects.
#'  \describe{
#'    \item{\code{possumarea:}}{A set of Cartesian coordinates defining the study area. The 
#'    variable \code{d.to.shore} in \code{possummask} is the distance from this line (except 
#'    the bottom boundary, which is not the shore).}
#'    \item{\code{possumremovalarea:}}{A set of Cartesian coordinates defining the area in 
#'    which possums were removed (the nominal removal area of Efford et al. (2005, Fig. 1).}
#'    \item{\code{possummask:}}{An \code{secr} mask object for the study.}
#'    \item{\code{possumCH:}}{An \code{secr} capture history object.}
#'    \item{\code{possum.model.0:}}{Fitted \code{secr} object using default model with 
#'    \code{D~1, g0~1, sigma~1}.}
#'    \item{\code{possum.model.b:}}{Fitted \code{secr} object using density model with 
#'    \code{D~1, g0~b, sigma~1}.}
#'    \item{\code{possum.model.Dh2:}}{Fitted \code{secr} object using density model with 
#'    \code{D~x + y + x2 + y2 + xy, g0~h2, sigma~h2, pmix~h2}.}
#'    \item{\code{possum.model.Dsh2:}}{Fitted \code{secr} object using density model with 
#'    \code{D~d.to.shore, g0~h2, sigma~h2, pmix~h2}.}
#'    \item{\code{possum.model.h2:}}{Fitted \code{secr} object using density model with 
#'    \code{D~1, g0~h2, sigma~h2, pmix~h2}.}
#'  }
#' @source Pakcage \code{secr}; origonally from Landcare Research, New Zealand.
NULL