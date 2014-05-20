\name{addcov2mask}
\alias{addcov2mask}
\title{Add covariates to mask from raster data}
\usage{
addcov2mask(mask, raster, layer.names = NULL, distance.to.layer = NULL,
  plot = TRUE, layer.cols = NULL)
}
\arguments{
  \item{mask}{description needed...}

  \item{raster}{description needed...}

  \item{layer.names}{description needed...}

  \item{distance.to.layer}{description needed...}

  \item{plot}{description needed...}

  \item{layer.cols}{description needed...}

  \item{...}{additional arguments to pass to
  \code{\link{prep4image}}}
}
\description{
Adds covaraiates to a habitat mask from a GIS landuse data
in raster format.
}
\details{
details needed...
}
\examples{
# load mask and raster data
data(Boland.leopards)
data(Boland.landuse.image)

# function defaults
op = par(no.readonly = TRUE)
par(mfrow = c(1,1), mar = c(2,2,2,2), oma = c(0,0,0,0))
newmask = addcov2mask(
  Boland.mask, Boland.landuse.image,
  layer.names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"),
  layer.cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
)
par(op)
head(attributes(newmask)$covariates)

# specify colors and calculate distance to water and urbarn landclasses
op = par(no.readonly = TRUE)
par(mfrow = c(1,3), mar = c(2,2,2,2), oma = c(0,0,0,0))
newmask = addcov2mask(
  Boland.mask, Boland.landuse.image,
  layer.names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"),
  layer.cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
  distance.to.layer = c("Urban","Water")
)
par(op)
head(attributes(newmask)$covariates)
}

