#' @title Image plot of mask object.
#'   
#' @description Creates an image plot with a legend of a covariate in a mask.
#'   
#' @param mask object of class \code{mask}.
#' @param covariate value of covariate on mask points: one of the column names of the data frame 
#' \code{covariates(mask)}.
#' @param contours logical to plot contours on top of image plot.
#' @param xlab label for x-axis. If NULL, prints "x".
#' @param ylab label for x-axis. If NULL, prints "y".
#' @param ppoly logical for whether the bounding polygon should be plotted (if mask has a ‘polygon’ attribute),
#' @param polycol color of bounding polygon line.
#' @param main main title of plot.
#' @param ... other arguments to \code{\link{image.plot}} 
#' (which includes arguments to \code{\link{image}}).
#'   
#' @details See \code{\link{image.plot}} and \code{\link{image}}.
#' @examples
#' data(Boland.leopards1)
#' data(possums)
#' 
#' # plot image of distance to urban areas, with contours, and plot traps on top of it
#' image.plot.mask(Boland.mask1,"dist2.Urban",asp=1,axes=FALSE,xlab="Easting",ylab="Northing",main="Distance to Urban")
#' plot(traps(Boland.CH1),add=TRUE)
#' 
#' # a plot with different colour scheme, no contours and 1:1 aspect ratio:
#' image.plot.mask(possummask,"d.to.shore",col=terrain.colors(30),contours=FALSE)
#' plot(traps(possumCH),add=TRUE)
#' @export

image.plot.mask=function(mask,covariate=NULL,contours=TRUE,xlab=NULL,ylab=NULL,main=NULL,ppoly=TRUE,polycol="red",...) {
  if(is.null(covariate)) stop("No covariate; use plot( ) rather than image.plot( ) to plot mask")
  if (ms(mask)) {
    lapply(mask,image.plot.mask,covariate=covariate,xlab=xlab,ylab=ylab,ppoly=ppoly,polycol=polycol,...)
  }
  else {
    pdat=prep4image(data.frame(x=mask$x,y=mask$y,z=covariates(mask)[,covariate]),plot=FALSE)
    if(is.null(xlab)) xlab=names(pdat)[1]
    if(is.null(ylab)) ylab=names(pdat)[2]
    if(is.null(main)) main=covariate
    image.plot(pdat,xlab=xlab,ylab=ylab,main=main,...)
    if(contours) contour(pdat,add=TRUE)
    if (!is.null(attr(mask, "polygon")) & ppoly) {
      poly <- attr(mask, "polygon")
      if (class(poly) == "SpatialPolygonsDataFrame") {
        plot(poly, add = TRUE)
      }
      else polygon(poly, col = polycol, density = 0)
    }
  }
}

