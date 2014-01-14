\name{add.covariate.bases}
\alias{add.covariate.bases}
\title{Add basis functions to attr(mask,"covariates").}
\usage{
add.covariate.bases(mask)
}
\arguments{
  \item{mask}{secr mask object.}
}
\description{
Adds all basis functions to attr(mask,"covariates"),
including linear (non-smoothed) explanatory variables, as a
single data frame. (This data frame is not currently used
but seems necessary for use with secr.fit.)
}

