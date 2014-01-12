\docType{data}
\name{WCape.alt}
\alias{WCape.alt}
\title{Altitudes of terrain in Western Cape, South Africa.}
\format{A data frame with the following elements $x, $y and $z, where $x and $y are longitude
and latitude and $z is altitude (in m).}
\source{
  www.ngdc.noaa.gov/mgg/global/
}
\description{
  .
}
\examples{
data(WCape.alt)
 # use \\code{\\link{prep4image}} to format for \\code{\\link{image}}.
 WCape.image=prep4image(WCape.alt,plot=TRUE,col=terrain.colors(30))
 contour(WCape.image,add=TRUE)
}
\references{
  Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global
  Relief Model: Procedures, Data Sources and Analysis. NOAA
  Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
}
\keyword{datasets}

