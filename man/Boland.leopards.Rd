\docType{data}
\name{Boland.leopards}
\alias{Boland.leopards}
\title{Data from 2010 camera-trap survey of leopards in Boland, Western Cape, South
Africa.}
\format{A list with the following three elements.
 \describe{
   \item{\code{$capthist}:}{ an \code{\link{secr}} capture history object.}
   \item{\code{$mask}:}{ an \code{\link{secr}} mask object for \code{Boland.cameras}.
   \code{attr($mask,"covariates")} is a data frame with altitude data in column
   \code{alt} and the product of longitude and altitude and of latitude and altitude in
   columns \code{xalt} and \code{yalt}, both scaled by dividing by their mean.}
   \item{\code{$image}:}{ altitude data for the \code{Boland.leopards} study region,
   comprising a list in a format suitable for plotting with \code{\link{image}}. It has three
   elements: $x, being a a vector of longitude values of a grid spanning the study area,
   $y, being a a vector of latitude values of a grid spanning the study area, and $z, being
   a matrix of altitudes, in format suitable for use with \code{\link{image}}.}
 }}
\source{
  \code{Boland.leopards$capthist} and
  \code{Boland.leopards$mask} were provided by the Cape
  Leopard Trust Boland Leopard Project
  (http://capeleopard.org.za/research/leopard/boland).
  \code{Boland.image} was downloaded from
  www.ngdc.noaa.gov/mgg/global/ (and then formatted using
  \code{\link{prep4image}})
}
\description{
  .
}
\examples{
data(Boland.leopards)
 Boland.image=Boland.leopards$image
 image(Boland.image,col=terrain.colors(60))
 contour(Boland.image,add=TRUE)
 Boland.cameras=traps(Boland.leopards$capthist)
 plot(Boland.cameras,add=TRUE,detpar=list(pch="+",cex=1.2))
}
\references{
  Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global
  Relief Model: Procedures, Data Sources and Analysis. NOAA
  Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
}
\keyword{datasets}

