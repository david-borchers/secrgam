\docType{data}
\name{secr.possum}
\alias{secr.possum}
\title{Data from a trapping study of brushtail possums at Waitarere, North
  Island, New Zealand.}
\format{An object containing the following data objects. \describe{
  \item{\code{possumarea:}}{A set of Cartesian coordinates defining the study
  area. The variable \code{d.to.shore} in \code{possummask} is the distance
  from this line (except the bottom boundary, which is not the shore).}
  \item{\code{possumremovalarea:}}{A set of Cartesian coordinates defining
  the area in which possums were removed (the nominal removal area of Efford
  et al. (2005, Fig. 1).} \item{\code{possummask:}}{An \code{secr} mask
  object for the study.} \item{\code{possumCH:}}{An \code{secr} capture
  history object.} \item{\code{possum.model.0:}}{Fitted \code{secr} object
  using default model with \code{D~1, g0~1, sigma~1}.}
  \item{\code{possum.model.b:}}{Fitted \code{secr} object using density model
  with \code{D~1, g0~b, sigma~1}.} \item{\code{possum.model.Dh2:}}{Fitted
  \code{secr} object using density model with \code{D~x + y + x2 + y2 + xy,
  g0~h2, sigma~h2, pmix~h2}.} \item{\code{possum.model.Dsh2:}}{Fitted
  \code{secr} object using density model with \code{D~d.to.shore, g0~h2,
  sigma~h2, pmix~h2}.} \item{\code{possum.model.h2:}}{Fitted \code{secr}
  object using density model with \code{D~1, g0~h2, sigma~h2, pmix~h2}.} }}
\source{
Pakcage \code{secr}; origonally from Landcare Research, New
Zealand.
}
\usage{
data(secr.possum)
}
\description{
These data are taken directly from package \code{secr}.
Type \code{help(possum)} for a detailed description of the
data.
}

