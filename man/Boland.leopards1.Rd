\docType{data}
\name{Boland.leopards1}
\alias{Boland.leopards1}
\title{Simulated data from 2010 camera-trap survey of leopards in the Boland, South
Africa.}
\format{An object containing list with the following two data objects.
 \describe{
   \item{\code{Boland.CH1}:}{ an \code{\link{secr}} capture history object with camera-trap
   detections of leopards in the form of binary data over 13 occasions.}
   \item{\code{Boland.mask1}:}{ an \code{\link{secr}} mask object for the data in \code{Boland.CH}.
   It contans the following covariates:
   \describe{
   \item{\code{alt}:}{altitude}
   \item{\code{Landuse}:}{Landuse category (1=Natural, 2=Cultivated, 3=Degraded, 4=Plantation)}
   \item{\code{Natural}:}{binary variable: 1=Natural land use category, 0=not}
   \item{\code{dist2.Urban}:}{distance to closest Urban land use category cell}
   \item{\code{dist2.Water}:}{distance to closest Water land use category cell}
   \item{\code{dist2.Natural}:}{distance to closest Natural land use category cell}
   \item{\code{LUfactor}:}{Landuse category as a factor variable}
   }
 }
}}
\source{
\code{Boland.mask1} was provided by the Cape Leopard Trust
Boland Leopard Project
(http://capeleopard.org.za/research/leopard/boland).
\code{Boland.CH1} was simulated using a model fitted to the
real data.
}
\usage{
data(Boland.leopards1)
}
\description{
The data are simulated from a camera-trap survey of
leopards in the Boland region of South Africa, using a fit
to the real data to create the simulated population.
Cameras were checked weekly and data recorded in binary
format (i.e. only whether or not an animal was detected on
each occasion, not the number of detections within
occasions).
}

