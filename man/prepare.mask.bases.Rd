\name{prepare.mask.bases}
\alias{prepare.mask.bases}
\title{Add B-splines to mask.}
\usage{
  prepare.mask.bases(model, mask)
}
\arguments{
  \item{model}{secr model specification list.}

  \item{mask}{secr mask object.}
}
\value{
  Returns NULL if the Density model in model has no smooth
  terms (of form "s(x,k=b)"), else returns mask object with
  attributes "bases" and "covariates" as described above.
}
\description{
  Replaces attr(mask,"covariates") with a data frame
  containing new variables comprising basis function values
  for smoopthed variables, toghether with non-smoothed
  variables (but excluding "x" and "y", which are in mask
  already).  Also adds a list of basis functions to
  attributes(mask)$bases, and the df of each basis in
  attributes(mask)$bases.df. The list attribtes(mask)$bases
  contains original variables as 1D matrix if the variable
  is not smoothed, and B-spline basis object (a >1D matrix
  with attributes givning knots, etc.) if it is smoothed.
  The vector attr(mask,"bases.df") contains the degrees of
  freedom of the smooths.
}
\details{
  (None as yet)
}

