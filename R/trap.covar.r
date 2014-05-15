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