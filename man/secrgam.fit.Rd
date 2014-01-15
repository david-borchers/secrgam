\name{secrgam.fit}
\alias{secrgam.fit}
\title{SECR fit with B-pline Density GAM.}
\usage{
secrgam.fit(capthist, model, mask, ...)
}
\arguments{
  \item{capthist}{capture history, as for function
  secr.fit().}

  \item{model}{model list, as for function secr.fit(), but
  allowing terms of the form `s(var,k=df)', where var is a
  mask coordinate (x or y) or a variable in
  attr(mask,"covariate") and df is the degrees of freedom
  of the B-spline smooth (i.e. the number of knots).}

  \item{mask,}{as for function secr.fit().}

  \item{...}{other arguments to secr.fit()}
}
\description{
Fits GAM using regression B-splines for density model. Does
this by creating B-spline basis functions (which are stored
in mask object) and then calling function secr() using
these.
}
\details{
Returns an object of class "secr", i.e., the same class of
object as is returned by secr.fit of package
\code{\link{secr}}.
}

