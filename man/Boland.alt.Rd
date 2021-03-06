% Generated by roxygen2 (4.0.1): do not edit by hand
\docType{data}
\name{Boland.alt}
\alias{Boland.alt}
\title{Altitudes in the Boland.}
\format{A data frame with the following elements $x, $y and $z, where $x and $y are longitude
and latitude and $z is altitude (in m).}
\source{
www.ngdc.noaa.gov/mgg/global/
}
\usage{
data(Boland.alt)
}
\description{
Altitudes of terrain in the Boland, South Africa.
}
\examples{
data(Boland.alt)
op = par(no.readonly = TRUE)
par(mfrow = c(1,1))
prep4image(Boland.alt, asp = 1)
par(op)
}
\references{
Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources
and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
}

