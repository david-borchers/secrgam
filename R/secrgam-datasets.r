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
NULL

#' @name WCape.alt.image
#' @title Altitudes in Western Cape (in fromat for plotting).
#' @docType data
#' @description Altitudes of terrain in the Western Cape, South Africa.
#' @usage data(WCape.alt)
#' @format A list with the following elements: $x (vector of unique longitudes, in increasing order), 
#' $y (vector of unique longitudes, in increasing order) and $z (matrix of altitudes)..
#' @source www.ngdc.noaa.gov/mgg/global/
#' @references
#' Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources 
#' and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009.
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
NULL
#' @name Boland.landuse
#' @title Land use class in the Boland.
#' @docType data
#' @description Land use class of terrain in the Boland, South Africa.
#' @usage data(Boland.landuse)
#' @format A data frame with the following elements $x, $y and $z, where $x and $y are longitude
#' and latitude and $z is land use category: 1="Natural", 2="Cultivated", 3="Degraded", 4="Urban",
#' 5="Water", 6="Plantation".
#' @source http://bgis.sanbi.org/landcover/project.asp
#' @references
#' http://bgis.sanbi.org/landcover/Landcover2009.pdf
NULL

#' @name Boland.landuse.image
#' @title Land use class in the Boland (in fromat for plotting).
#' @docType data
#' @description Land use class of terrain in the the Boland, South Africa.
#' @usage data(Boland.landuse.image)
#' @format A list with the following elements: $x (vector of unique longitudes, in increasing order), 
#' $y (vector of unique longitudes, in increasing order) and $z (matrix of land use 
#' categories: 1="Natural", 2="Cultivated", 3="Degraded", 4="Urban", 5="Water", 
#' 6="Plantation").
#' @source http://bgis.sanbi.org/landcover/project.asp
#' @references
#' http://bgis.sanbi.org/landcover/Landcover2009.pdf
NULL


#' @name Boland.leopards
#' @title Data from 2010 camera-trap survey of leopards in the Boland, South 
#' Africa.
#' @docType data
#' @description The data are from a camera-trap survey of leopards in the Boland region of 
#' South Africa. Cameras were checked weekly and data recorded in binary format (i.e. only 
#' whether or not an animal was detected on each occasion, not the number of detections
#' within occasions). 
#' @usage data(Boland.leopards)
#' @format An object containing list with the following two data objects.
#'  \describe{
#'    \item{\code{Boland.CH}:}{ an \code{\link{secr}} capture history object with camera-trap 
#'    detections of leopards in the form of binary data over 13 occasions.}
#'    \item{\code{Boland.mask}:}{ an \code{\link{secr}} mask object for the data in \code{Boland.CH}.
#'    It contans the following covariates:
#'    \describe{
#'    \item{\code{alt}:}{altitude}
#'    \item{\code{xalt}:}{product of x and altitude, scaled by dividing by its mean}
#'    \item{\code{yalt}:}{product of y and altitude, scaled by dividing by its mean}
#'    \item{\code{Natural}:}{binary variable: 1=Natural land use category, 0=not}
#'    \item{\code{Cultivated}:}{binary variable: 1=Cultivated land use category, 0=not}
#'    \item{\code{Degraded}:}{binary variable: 1=Degraded land use category, 0=not}
#'    \item{\code{Plantation}:}{binary variable: 1=Plantation land use category, 0=not}
#'    \item{\code{dist2.Urban}:}{distance to closest Urban land use category cell}
#'    \item{\code{dist2.Water}:}{distance to closest Water land use category cell}
#'    \item{\code{dist2.Natural}:}{distance to closest Natural land use category cell}
#'    \item{\code{Landuse}:}{Landuse category (1=Natural, 2=Cultivated, 3=Degraded, 4=Plantation)}
#'    \item{\code{lufactor}:}{Landuse category as a factor variable}
#'    }
#'  }
#'}
#' @source \code{Boland.CH} and \code{Boland.mask} were provided by the Cape Leopard Trust 
#' Boland Leopard Project (http://capeleopard.org.za/research/leopard/boland). 
NULL
