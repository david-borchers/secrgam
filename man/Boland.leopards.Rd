\docType{data}
\name{Boland.leopards}
\alias{Boland.leopards}
\title{Data from 2010 camera-trap survey of leopards in the Boland, South
  Africa.}
\format{An object containing list with the following three data objects.
  \describe{ \item{\code{Boland.CH}:}{ an \code{\link{secr}} capture history
  object with camera-trap detections of leopards in the form of binary data
  over 13 occasions.} \item{\code{Boland.mask}:}{ an \code{\link{secr}} mask
  object for the data in \code{Boland.CH}. In
  \code{attr(Boland.mask,"covariates")} is a data frame with altitude data in
  column \code{alt} and the product of longitude and altitude and of latitude
  and altitude in columns \code{xalt} and \code{yalt}, both scaled by
  dividing by their mean.} \item{\code{Boland.image}:}{ altitude data for the
  \code{Boland.leopards} study region, comprising a list in a format suitable
  for plotting with \code{\link{image}}. It has three elements: $x, being a a
  vector of longitude values of a grid spanning the study area, $y, being a a
  vector of latitude values of a grid spanning the study area, and $z, being
  a matrix of altitudes, in format suitable for use with
  \code{\link{image}}.} }}
\source{
\code{Boland.CH} and \code{Boland.mask} were provided by
the Cape Leopard Trust Boland Leopard Project
(http://capeleopard.org.za/research/leopard/boland).

\code{Boland.image} was downloaded from
www.ngdc.noaa.gov/mgg/global/ (and then formatted using
\code{\link{prep4image}}).
}
\usage{
data(Boland.leopards)
}
\description{
The data are from a camera-trap survey of leopards in the
Boland region of South Africa. Cameras were checked weekly
and data recorded in binary format (i.e. only whether or
not an animal was detected on each occasion, not the number
of detections within occasions).
}
\references{
Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global
Relief Model: Procedures, Data Sources and Analysis. NOAA
Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
}

