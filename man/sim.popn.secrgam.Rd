\name{sim.popn.secrgam}
\alias{sim.popn.secrgam}
\title{Generate a population using any density formula}
\usage{
sim.popn.secrgam(fit = NULL, N = NULL, Dmodel = NULL, Dpars = NULL,
  Dlink = "log", mask = NULL)
}
\arguments{
  \item{fit}{a fitted \code{\link{secrgam}} or
  \code{\link{secr}} model}

  \item{N}{average abundance (converts density surface to
  relative density)}

  \item{Dmodel}{density model formula}

  \item{Dpars}{associated parameters for the density model}

  \item{Dlink}{link function (default is "log")}

  \item{mask}{\code{\link{mask}} object (as used by
  \code{\link{secrgam.fit}} and \code{\link{secr.fit}})}

  \item{log}{if \code{TRUE} the log link is used, if
  \code{FALSE} the identity link is used}
}
\value{
value...
}
\description{
description...
}
\details{
details...
}
\examples{
data(Boland.leopards1)

# use a fitted model
popn = sim.popn.secrgam(fit1.a3) ; head(popn) ; dim(popn)
plot(fit1.a3, asp = 1)
points(popn, cex = 0.5, pch = 19)

# use an inflated density surface from a fitted model
popn = sim.popn.secrgam(fit1.a3, N = 1000) ; head(popn) ; dim(popn)
plot(fit1.a3, asp = 1)
points(popn, cex = 0.5, pch = 19)

\dontrun{
# use an alternative model
# e.g. construct a tensor product model to represent a bivariate normal
require(mvtnorm)
D = dmvnorm(Boland.mask, apply(Boland.mask, 2, mean), 10e7 * diag(2)) * 10e6
N = sum(D) * attributes(Boland.mask)$area ; N # expected sample size
data = cbind(z = D, Boland.mask)
prep4image(data, key = FALSE, col = topo.colors(10), asp = 1)
model = D ~ te(x, y, k = 3)
fit = gam(model, gaussian(link = "log"), data, fx = TRUE)
prep4image(data.frame(x = data$x, y = data$y, z = fitted(fit)), key = FALSE, asp = 1)
popn = sim.popn.secrgam(Dmodel = model, Dpars = coef(fit), mask = Boland.mask) ; head(popn) ; dim(popn)
points(popn, cex = 0.5, pch = 19)
}
}

