% Generated by roxygen2 (4.0.1): do not edit by hand
\name{secrgam.fit}
\alias{secrgam.fit}
\title{SECR fit with flexible Density models - beta version.}
\usage{
secrgam.fit(capthist, model = list(D ~ 1, g0 ~ 1, sigma ~ 1), mask = NULL,
  buffer = 100, sessioncov = NULL, ...)
}
\arguments{
\item{capthist}{capture history, as for function \code{secr.fit}.}

\item{model}{model list, as for function \code{secr.fit}, but allowing terms
of the form \code{s(var,k=df)},  \code{s(var,...,k=df)},  \code{te(var,...,k=df)}
where var is a mask coordinate (x or y) or a variable in \code{attr(mask, "covariate")},
``...'' is a comma-separated list of other such variables, and df is the degrees
of freedom of the spline basis (the number of knots).}

\item{mask,}{as for function \code{secr.fit}}

\item{...}{other arguments to \code{secr.fit}}
}
\description{
Fits GAM using regression splines for the density model. Does this
  by creating spline basis functions (which are stored as covariates in a mask object)
  and then calling function \code{\link{secr.fit}} to operate on these.
}
\details{
Returns an object of class c("secrgam","secr"), the latter being same class of
  object as is returned by \code{secr.fit} of package \code{\link{secr}}.
}
\examples{
\dontrun{
data(Boland.leopards1)
model = list(D ~ s(alt, k = 4), g0 ~ 1, sigma ~ 1)
fit = secrgam.fit_beta(capthist = Boland.CH1, model = model, mask = Boland.mask1, trace = FALSE)
fit # look at fit results

# plot fitted surface:
plot(fit, asp=1)
plot(traps(Boland.CH1), add = TRUE)

# plot smooths:
plot(fit, type = "smooth")

# get abundance and density estimates
region.ND(fit)

# session-level smooths
data(ovenbird)
ovenfit = secrgam.fit(ovenCH, model = D ~ s(Session,k=3), mask = ovenmask, trace = FALSE)
plot(ovenfit, type = "smooth")
region.ND(ovenfit)
}
}

