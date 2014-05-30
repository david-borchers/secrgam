\name{AIC.secrgam}
\alias{AIC.secrgam}
\title{Compare SECRgam models.}
\usage{
\method{AIC}{secrgam}(object, ..., sort = TRUE, k = 2, dmax = 10,
  criterion = c("AICc", "AIC"))
}
\arguments{
  \item{object}{secr object output from the function
  secr.fit, secrgam.fit, or a list of such objects with
  class c("list","secrlist").}

  \item{...}{other secr objects}

  \item{sort}{logical for whether rows should be sorted by
  ascending AICc}

  \item{k}{numeric, penalty per parameter to be used;
  always k = 2 in this method}

  \item{dmax}{numeric, maximum AIC difference for inclusion
  in confidence set}

  \item{criterion}{character, criterion to use for model
  comparison and weights.}
}
\description{
Terse report on the fit of one or more spatially explicit
capture–recapture models. Models with smaller values of AIC
(Akaike's Information Criterion) are preferred.
}
\details{
Same as the function of same name in package \code{secr},
but also takes objects of class \code{secrgam} as input.
See \code{help(secr:::AIC.secr)} for details.
}

