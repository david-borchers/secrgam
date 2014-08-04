#---------------------------------- Datasets ------------------------------------
#' @name WCape.alt
#' @title Altitudes in Western Cape.
#' @docType data
#' @description Altitudes of terrain in the Western Cape, South Africa.
#' @usage data(WCape.alt)
#' @format A data frame with the following elements $x, $y and $z, where $x and $y are longitude
#' and latitude and $z is altitude (in m).
#' @source www.ngdc.noaa.gov/mgg/global/
#' @references
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources 
#' and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
#' @examples
#' data(WCape.alt)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,1))
#' prep4image(WCape.alt, asp = 1)
#' par(op)
NULL

#' @name WCape.alt.image
#' @title Altitudes in Western Cape (in fromat for plotting).
#' @docType data
#' @description Altitudes of terrain in the Western Cape, South Africa.
#' @usage data(WCape.alt.image)
#' @format A list with the following elements: $x (vector of unique longitudes, in increasing order), 
#' $y (vector of unique longitudes, in increasing order) and $z (matrix of altitudes)..
#' @source www.ngdc.noaa.gov/mgg/global/
#' @references
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources 
#' and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
#' @examples
#' data(WCape.alt.image)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,1))
#' image(WCape.alt.image, asp = 1)
#' contour(WCape.alt.image, add = TRUE)
#' par(op)
NULL

#' @name Boland.alt
#' @title Altitudes in the Boland.
#' @docType data
#' @description Altitudes of terrain in the Boland, South Africa.
#' @usage data(Boland.alt)
#' @format A data frame with the following elements $x, $y and $z, where $x and $y are longitude
#' and latitude and $z is altitude (in m).
#' @source www.ngdc.noaa.gov/mgg/global/
#' @references
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources 
#' and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
#' @examples
#' data(Boland.alt)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,1))
#' prep4image(Boland.alt, asp = 1)
#' par(op)
NULL

#' @name Boland.alt.image
#' @title Altitudes in the Boland (in fromat for plotting).
#' @docType data
#' @description Altitudes of terrain in the the Boland, South Africa.
#' @usage data(Boland.alt.image)
#' @format A list with the following elements: $x (vector of unique longitudes, in increasing order), 
#' $y (vector of unique longitudes, in increasing order) and $z (matrix of altitudes)..
#' @source www.ngdc.noaa.gov/mgg/global/
#' @references
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources 
#' and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
#' @examples
#' data(Boland.alt.image)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,1))
#' image(Boland.alt.image, asp = 1)
#' contour(Boland.alt.image, add = TRUE)
#' par(op)
NULL

#' @name Boland.landuse
#' @title Land use class in the Boland.
#' @docType data
#' @description Land use class of terrain in the Boland, South Africa.
#' @usage data(Boland.landuse)
#' @format A data frame with the following elements \code{x}, \code{y} and
#'   \code{landusage}, where \code{x} and \code{y} are longitude and latitude and
#'   \code{landusage} is land use category: 1="Natural", 2="Cultivated",
#'   3="Degraded", 4="Urban", 5="Water", 6="Plantation".
#' @source http://bgis.sanbi.org/landcover/project.asp
#' @references http://bgis.sanbi.org/landcover/Landcover2009.pdf
#' @seealso \code{\link{Boland.landuse.image}}
NULL

#' @name Boland.landuse.image
#' @title Land use class in the Boland (in fromat for plotting).
#' @docType data
#' @description Land use class of terrain in the the Boland, South Africa.
#' @usage data(Boland.landuse.image)
#' @format A list with the following elements: \code{x} (vector of unique longitudes, in increasing order), 
#' \code{y} (vector of unique longitudes, in increasing order) and \code{landuse} (matrix of land use 
#' categories: 1="Natural", 2="Cultivated", 3="Degraded", 4="Urban", 5="Water", 
#' 6="Plantation").
#' @source http://bgis.sanbi.org/landcover/project.asp
#' @references
#' http://bgis.sanbi.org/landcover/Landcover2009.pdf
#' @examples
#' \dontrun{
#' data(Boland.landuse.image)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,1))
#' image(Boland.landuse.image, asp = 1, col = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"))
#' par(op)
#' }
NULL

#' @name Boland.leopards1
#' @aliases Boland.mask1 Boland.CH1
#' @title Simulated data from 2010 camera-trap survey of leopards in the Boland, South 
#' Africa.
#' @docType data
#' @description The data are simulated from a camera-trap survey of leopards in the Boland 
#' region of South Africa, using a fit to the real data to create the simulated population. 
#' Cameras were checked weekly and data recorded in binary format (i.e. only 
#' whether or not an animal was detected on each occasion, not the number of detections
#' within occasions). 
#' @usage data(Boland.leopards1)
#' @format An object containing list with the following two data objects.
#'  \describe{
#'    \item{\code{Boland.CH1}:}{ an \code{\link{secr}} capture history object with camera-trap 
#'    detections of leopards in the form of binary data over 13 occasions.}
#'    \item{\code{Boland.mask1}:}{ an \code{\link{secr}} mask object for the data in \code{Boland.CH}.
#'    It contans the following covariates:
#'    \describe{
#'    \item{\code{alt}:}{altitude}
#'    \item{\code{Landuse}:}{Landuse category (1=Natural, 2=Cultivated, 3=Degraded, 4=Plantation)}
#'    \item{\code{Natural}:}{binary variable: 1=Natural land use category, 0=not}
#'    \item{\code{dist2.Urban}:}{distance to closest Urban land use category cell}
#'    \item{\code{dist2.Water}:}{distance to closest Water land use category cell}
#'    \item{\code{dist2.Natural}:}{distance to closest Natural land use category cell}
#'    \item{\code{LUfactor}:}{Landuse category as a factor variable}
#'    }
#'  }
#'}
#' @source \code{Boland.mask1} was provided by the Cape Leopard Trust Boland Leopard Project 
#' (http://capeleopard.org.za/research/leopard/boland). \code{Boland.CH1} was simulated using 
#' a model fitted to the real data.
NULL

#' @name Boland.leopards2
#' @aliases Boland.mask2 Boland.CH2
#' @title Simulated data from a more extensive simulated survey than the real 
#' 2010 camera-trap survey of leopards in the Boland, South Africa.
#' @docType data
#' @description The data are simulated from a camera-trap survey of leopards in the Boland 
#' region of South Africa, using a fit to the real data to create the simulated population, 
#' but using a more extensive array of (simulated) camera traps than was used on the real survey. 
#' Cameras were checked weekly and data recorded in binary format (i.e. only 
#' whether or not an animal was detected on each occasion, not the number of detections
#' within occasions). 
#' @usage data(Boland.leopards2)
#' @format An object containing list with the following two data objects.
#'  \describe{
#'    \item{\code{Boland.CH2}:}{ an \code{\link{secr}} capture history object with camera-trap 
#'    detections of leopards in the form of binary data over 13 occasions.}
#'    \item{\code{Boland.mask2}:}{ an \code{\link{secr}} mask object for the data in \code{Boland.CH}.
#'    It contans the following covariates:
#'    \describe{
#'    \item{\code{alt}:}{altitude}
#'    \item{\code{Landuse}:}{Landuse category (1=Natural, 2=Cultivated, 3=Degraded, 4=Plantation)}
#'    \item{\code{Natural}:}{binary variable: 1=Natural land use category, 0=not}
#'    \item{\code{dist2.Urban}:}{distance to closest Urban land use category cell}
#'    \item{\code{dist2.Water}:}{distance to closest Water land use category cell}
#'    \item{\code{dist2.Natural}:}{distance to closest Natural land use category cell}
#'    \item{\code{LUfactor}:}{Landuse category as a factor variable}
#'    }
#'  }
#'}
#' @source \code{Boland.CH2} and \code{Boland.mask2} were simulated from data provided by the 
#' Cape Leopard Trust Boland Leopard Project (http://capeleopard.org.za/research/leopard/boland). 
NULL

#' @name Boland.fits1
#' @title SECR  models fitted to \code{Boland.CH1}.
#' @docType data
#' @description Models with nonhomogeneous Poisson Process models for density, fitted to 
#' \code{Boland.CH1}.
#' @usage data(Boland.fits1)
#' @format An object containing list with the following objects of class \code{secrgam}.
#'  \describe{
#'    \item{\code{fit1.0}:}{ an \code{\link{secrgam}} object with homogeneous density.}
#'    \item{\code{fit1.a3}:}{ an \code{\link{secrgam}} object with fitted density being a 
#'    smooth of altitude (\code{alt}), with 3 degrees of freedom.}
#'    \item{\code{fit1.a4}:}{ an \code{\link{secrgam}} object with fitted density being a 
#'    smooth of altitude (\code{alt}), with 4 degrees of freedom.}
#'    \item{\code{fit1.a3.dW3}:}{ an \code{\link{secrgam}} object with fitted density being a 
#'    smooth of altitude (\code{alt}), with 3 degrees of freedom and distance from water (\code{dist2.Water}),
#'    with 3 degrees of freedom.}
#'  }
NULL

#' @name Boland.fits2
#' @title SECR  models fitted to \code{Boland.CH2}.
#' @docType data
#' @description Models with nonhomogeneous Poisson Process models for density, fitted to 
#' \code{Boland.CH2}.
#' @usage data(Boland.fits1)
#' @format An object containing list with the following objects of class \code{secrgam}.
#'  \describe{
#'    \item{\code{fit2.0}:}{ an \code{\link{secrgam}} object with homogeneous density.}
#'    \item{\code{fit2.a3.dW3}:}{ an \code{\link{secrgam}} object with fitted density being 
#'    smooths of altitude (\code{alt}) with 3 degrees of freedom and distance from water 
#'    (\code{dist2.Water}) with 3 degrees of freedom.}
#'    \item{\code{fit2.N.a3.dW3}:}{ an \code{\link{secrgam}} object with fitted density being a 
#'    linear functoin of the factor \code{Natural}, and smooths of altitude (\code{alt}) with 3 
#'    degrees of freedom and distance from water (\code{dist2.Water}) with 3 degrees of freedom.}
#'  }
NULL
