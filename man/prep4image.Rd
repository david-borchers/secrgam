\name{prep4image}
\alias{prep4image}
\title{Prepares data frame for plotting with image/contour/persp.}
\usage{
  prep4image(obj, plot = FALSE, ...)
}
\arguments{
  \item{obj}{a data frame with columns x and y being
  Cartesian coordinates on a REGULAR RECTANGULAR GRID, and
  z being the values of some variable at each coordinate.}

  \item{plot}{A logical indicating whether or not an image
  plot should be created (by way of checking the output of
  this function).}

  \item{...}{other arguments to \code{\link{image}}; only
  used if plot==TRUE.}
}
\description{
  From an input data frame with columns x, y and z, this
  function creates a list with elements x, y and z in
  format suitable for passing to functions image, contour
  or persp.
}
\details{
  Sorts z on values of x first, then y, then creates a
  matrix of z-values from this. Returns a list with elemets
  x (unique values of x, in increasing order), y (unique
  values of y, in increasing order) and z (matrix of
  z-values in appropriate order for image/contour/persp).
}
\examples{
data(WCape.alt)
WCape.image=prep4image(WCape.alt,plot=TRUE,col=terrain.colors(30))
contour(WCape.image,add=TRUE)
}

