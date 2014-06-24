\name{AIC.secrlist}
\alias{AIC.secrlist}
\title{Prints AIC table using AIC.secr output as input.}
\usage{
\method{AIC}{secrlist}(object, ..., sort = TRUE, k = 2, dmax = 10,
  criterion = c("AICc", "AIC"))
}
\arguments{
  \item{object}{secr object output from the function
  secr.fit, or a list of such objects with class
  c("list","secrlist").}

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
Same as the function of same name in package \code{secr},
but with explicit call to secr:::oneline.secr rather than
just onleline.secr.
}
\details{
see \code{AIC.secr}
}

