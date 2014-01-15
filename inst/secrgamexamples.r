#' @examples
#' data(Boland.leopards)
#' # make a model with dependence on altitude via smooth with 2 degrees of freedom
#' model=list(D~s(alt,k=2),g0~1,sigma~1)
#' # fit model
#' fit=secrgam.fit(capthist=Boland.CH,model=model,mask=Boland.mask,trace=FALSE)
#' fit # look at fit results
#' plotDgam(fit)
plotDgam=function(fit,type="link",mask.rug=FALSE,det.rug=TRUE,npts=200,show.knots=TRUE)


#' @examples
#'  data(Boland.leopards)
#'  image(Boland.image,col=terrain.colors(60))
#'  contour(Boland.image,add=TRUE)
#'  Boland.cameras=traps(Boland.CH)
#'  plot(Boland.cameras,add=TRUE,detpar=list(pch="+",cex=1.2))
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
#' @references
#' # (To come)
NULL


