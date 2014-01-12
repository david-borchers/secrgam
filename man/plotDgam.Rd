\name{plotDgam}
\alias{plotDgam}
\title{SECR Density B-pline plotting.}
\usage{
  plotDgam(fit, type = "link", mask.rug = FALSE,
    det.rug = TRUE, npts = 200, show.knots = TRUE)
}
\arguments{
  \item{fit}{object returned by secr.fit().}

  \item{type}{either "link" (for plot on link funtion
  scale) or "response" (for plot on response scale).}

  \item{mask.rug}{TRUE if a rugplot of locations of mask
  points in the covariate dimension is to be plotted.}

  \item{det.rug}{TRUE if a rugplot of locations of
  detectors in the covariate dimension is to be plotted.}

  \item{npts}{number of points on x-axis at which to
  evaluate smooth.}

  \item{show.knots}{if TRUE, puts crosses on smooth at knot
  locations.}
}
\description{
  Plots B-spline smooth components of an secr density model
}
\details{
  (None as yet)
}
\examples{
data(Boland.leopards)
Boland.capthist=Boland.leopards$capthist
Boland.mask=Boland.leopards$mask
# make a model with dependence on altitude via smooth with 2 degrees of freedom
model=list(D~s(alt,k=2),g0~1,sigma~1)
# fit model
fit=secr.fit.gam(capthist=Boland.capthist,model=model,mask=Boland.mask,trace=FALSE)
fit # look at fit results
plotDgam(fit)
}

