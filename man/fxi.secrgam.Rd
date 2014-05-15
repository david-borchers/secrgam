\name{fxi.secrgam}
\alias{fxi.secrgam}
\title{Probability Density of Home Range Centre}
\usage{
fxi.secrgam(object, X = NULL, ...)
}
\arguments{
  \item{object}{a fitted secrgam model (returned from
  \code{\link{secrgam.fit}})}

  \item{X}{2-column matrix of x- and y- coordinates (e.g.
  the original mask)}

  \item{...}{additional arguments passed to
  \code{\link{fxi.secr}} (i.e.  \code{i}, \code{sessnum} or
  \code{normal})}
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

