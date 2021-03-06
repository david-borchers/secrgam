% Generated by roxygen2 (4.0.1): do not edit by hand
\name{prepare.mask.bases}
\alias{prepare.mask.bases}
\title{Add spline basis functions to mask covariates.}
\usage{
prepare.mask.bases(Dmodel, mask, sessioncov = NULL, nsessions = 1)
}
\arguments{
\item{model}{secr model specification list.}

\item{mask}{secr mask object.}
}
\value{
Returns a \code{mask} object with updated covariates.
}
\description{
Replaces attr(mask,"covariates") with a data frame containing
new variables comprising basis function values for smoopthed variables,
toghether with non-smoothed variables (but excluding "x" and "y", which are
in mask already).
}
\details{
(None as yet...)
}
\examples{
data(Boland.leopards1)
head(Boland.mask1)
head(covariates(Boland.mask1))
newmask = prepare.mask.bases(~s(x, k = 3), Boland.mask1)
head(newmask)
head(covariates(newmask))
}

