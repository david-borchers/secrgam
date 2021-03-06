% Generated by roxygen2 (4.0.1): do not edit by hand
\docType{data}
\name{WCape.alt.image}
\alias{WCape.alt.image}
\title{Altitudes in Western Cape (in fromat for plotting).}
\format{A list with the following elements: $x (vector of unique longitudes, in increasing order),
$y (vector of unique longitudes, in increasing order) and $z (matrix of altitudes)..}
\source{
www.ngdc.noaa.gov/mgg/global/
}
\usage{
data(WCape.alt.image)
}
\description{
Altitudes of terrain in the Western Cape, South Africa.
}
\examples{
data(WCape.alt.image)
op = par(no.readonly = TRUE)
par(mfrow = c(1,1))
image(WCape.alt.image, asp = 1)
contour(WCape.alt.image, add = TRUE)
par(op)
}
\references{
Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources
and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
}

