\name{fxi.secrgam}
\alias{fxi.secrgam}
\title{Probability Density of Home Range Centre}
\usage{
fxi.secrgam(object, i = 1, col = 1:length(i), X = NULL, plt = TRUE,
  add = FALSE, sessnum = 1, border = 0, normal = TRUE, ...)
}
\arguments{
  \item{object}{a fitted secrgam model (returned from
  \code{\link{secrgam.fit}})}

  \item{i}{integer or character vector of individuals for
  which to plot contours}

  \item{col}{integer or character vector of colour for
  individuals' contours}

  \item{X}{2-column matrix of x- and y- coordinates (e.g.
  the original mask)}

  \item{plt}{logical to do contour plot}

  \item{add}{logical to add to existing plot}

  \item{sessnum}{session number if object$capthist spans
  multiple sessions}

  \item{border}{width of blank margin around the outermost
  detectors}

  \item{normal}{logical; should values of pdf be
  normalised?}

  \item{...}{additional arguments passed to contour.}
}
\description{
Calculates the probability density function for the
estimated location of one or more range centres, i.e.
\eqn{f(X | \Omega)}. Acts as a wrapper function for
\code{\link{fxi.secr}}.
}
\details{
\code{fxi.secr} returns a vector of probability densities
assuming a uniform density distribution,

\deqn{f(x_i | \omega_i) = P(\omega_i | x_i) / \sum
P(\omega_i | x_i)}

\code{fxi.secrgam} returns a vector of probability
densities for any density distribution,

\deqn{f(x_i | \omega_i) = P(\omega_i | x_i) D(x_i) / \sum [
P(\omega_i | x_i) D(x_i) ]}

Needs to be tested for cases where density model has fixed
parameters. Also assumes that log link is used for density.
}

