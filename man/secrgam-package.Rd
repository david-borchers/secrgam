\name{secrgam-package}
\alias{secrgam-package}
\alias{secrgam}
\docType{package}
\title{
SECR with Density Surface GAMs
}
\description{
Implements Genaralized Additive Models for modelling density surfaces for Spatially Explicit
Capture-Recapture, by means of regression splines.
}
\details{
\tabular{ll}{
Package: \tab secrgam\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-01-12\cr
License: \tab GNU General Public License Version 2 or later\cr
}
Package \code{secrgam} uses regression splines to implement flexible density surface estimation for Spatially Explicit Capture-Recapture methods, using maximum likelihood inference. At its core is package \code{\link{secr}}, which does all the actual estimation; package \code{secrgam} is just a set of wrapper functions and some plotting functions that set up the regression spline bases and then call \code{\link{secr}}.\cr

The package currently implements only additive 1-dimensional regression splines (B-splines).
}
\author{
David Borchers

Maintainer: David Borchers <dlb@st-andrews.ac.uk>
}
\keyword{spatially explicit, capture-recapture, regression spline, generalized additive model}
\seealso{
\code{\link{secr}}
}
\examples{
library(secrgam)

data(Boland.leopards)

# plot survey region and traps
Boland.image=Boland.leopards$image
Boland.capthist=Boland.leopards$capthist
Boland.cameras=traps(Boland.capthist)
image(Boland.image,col=terrain.colors(60))
contour(Boland.image,add=TRUE)
plot(Boland.cameras,add=TRUE,detpar=list(pch="+",cex=1.2))

# summarise and plot capture histories
summary(Boland.capthist)
plot(Boland.capthist,border=0,rad=0.01,tracks=TRUE,icolour=colors()[seq(2,202,10)])

# make a model with dependence on altitude via smooth with 2 degrees of freedom
model=list(D~s(alt,k=2),g0~1,sigma~1)
Boland.mask=Boland.leopards$mask
# fit model (and look at how long it takes)
system.time(fit<-secr.fit.gam(capthist=Boland.capthist,model=model,mask=Boland.mask,trace=FALSE))
fit # look at fit results
# plot fitted surface:
Dsurface <- predictDsurface(fit)
plot(Dsurface,border=0, polyc='blue', plottype='shaded',col=terrain.colors(30), breaks=25, meshcol=NA)
plot(traps(Boland.capthist), add=TRUE)
# plot smooths:
plotDgam(fit)

}