\name{plotDgam}
\alias{plotDgam}
\title{SECR Density spline plotting.}
\usage{
plotDgam(fit, type = "link", mask.rug = FALSE, det.rug = TRUE,
  npts = 200, show.knots = FALSE)
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
  locations. NOT YET IMPLEMENTED.}
}
\description{
Plots B-spline smooth components of an secr density model
}
\details{
(None as yet)
}

