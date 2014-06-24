\name{image.plot.mask}
\alias{image.plot.mask}
\title{Image plot of mask object.}
\usage{
image.plot.mask(mask, covariate = NULL, contours = TRUE, xlab = NULL,
  ylab = NULL, main = NULL, ppoly = TRUE, polycol = "red", ...)
}
\arguments{
  \item{mask}{object of class \code{mask}.}

  \item{covariate}{value of covariate on mask points: one
  of the column names of the data frame
  \code{covariates(mask)}.}

  \item{contours}{logical to plot contours on top of image
  plot.}

  \item{xlab}{label for x-axis. If NULL, prints "x".}

  \item{ylab}{label for x-axis. If NULL, prints "y".}

  \item{ppoly}{logical for whether the bounding polygon
  should be plotted (if mask has a ‘polygon’ attribute),}

  \item{polycol}{color of bounding polygon line.}

  \item{main}{main title of plot.}

  \item{...}{other arguments to \code{\link{image.plot}}
  (which includes arguments to \code{\link{image}}).}
}
\description{
Creates an image plot with a legend of a covariate in a
mask.
}
\details{
See \code{\link{image.plot}} and \code{\link{image}}.
}
\examples{
library(secrgam)

data(Boland.leopards)
data(possums)

# plot image of distance to urban areas, with contours, and plot traps on top of it
image.plot.mask(Boland.mask,"dist2.Urban",asp=1,axes=FALSE,xlab="Easting",ylab="Northing",main="Distance to Urban")
plot(traps(Boland.CH),add=TRUE)
# a plot with different colour scheme, no contours and 1:1 aspect ratio:
image.plot.mask(possummask,"d.to.shore",col=terrain.colors(30),contours=FALSE)
plot(traps(possumCH),add=TRUE)
}

