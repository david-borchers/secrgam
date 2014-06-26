
#' @name secrgam-package
#' @aliases secrgam
#' @docType package
#' @title SECR with Density Surface GAMs
#' @description Implements Genaralized Additive Models for modelling density 
#'   surfaces for Spatially Explicit Capture-Recapture, by means of regression 
#'   splines.
#' @details \tabular{ll}{ Package: \tab secrgam \cr Type: \tab Package \cr 
#'   Version: \tab 1.0 \cr Date: \tab 2014-01-12 \cr License: \tab GNU General 
#'   Public License Version 2 or later \cr }
#'   
#'   Package \code{secrgam} uses regression splines to implement flexible 
#'   density surface estimation for Spatially Explicit Capture-Recapture 
#'   methods, using maximum likelihood inference. At its core is package 
#'   \code{\link{secr}}, which does all the actual estimation; package 
#'   \code{secrgam} is just a set of wrapper functions and some plotting 
#'   functions that set up the regression spline bases and then call 
#'   \code{\link{secr}}.\cr
#'   
#'   The package currently allows spline models to be specified using syntax 
#'   from package \code{\link{mgcv}} using the \code{\link{s}} and 
#'   \code{\link{te}} functions. The \code{\link{gam}} function is used with
#'   \code{fit=FALSE} to obtain regression spline design matrices.
#'   
#' @author David Borchers \email{dlb@@st-andrews.ac.uk}
#' @author Darren Kidney \email{darrenkidney@@googlemail.com}
#'   
#'   Maintainer: David Borchers \email{dlb@@st-andrews.ac.uk}
#'   
#' @keywords spatially explicit, capture-recapture, regression spline, 
#'   generalized
#'   
#' @seealso \code{\link{secr}}
#' @examples
#' library(secrgam)
#' 
#' data(Boland.leopards1)
#' data(Boland.alt.image)
#' 
#' # plot survey region and traps
#' Boland.cameras = traps(Boland.CH1)
#' image(Boland.alt.image, col = terrain.colors(60),main="Altitude map")
#' contour(Boland.alt.image, add = TRUE)
#' plot(Boland.cameras, add = TRUE, detpar = list(pch = "+", cex = 1.2))
#' 
#' # summarise and plot capture histories
#' summary(Boland.CH1)
#' plot(Boland.CH1, border = 0, rad = 0.01, tracks = TRUE, icolour = colors()[seq(2, 202, 10)])
#' 
#' # make a model with dependence on altitude via smooth with 4 degrees of freedom
#' model = list(D ~ s(alt, k = 4), g0 ~ 1, sigma ~ 1)
#' 
#' # fit model
#' \dontrun{
#' fit <- secrgam.fit(capthist = Boland.CH1, model = model, mask = Boland.mask1, trace = FALSE)
#' fit # look at fit results
#' 
#' # plot fitted surface:
#' plot(fit,asp=1)
#' plot(traps(Boland.CH1), add = TRUE)
#' 
#' # plot smooths:
#' plotDgam(fit)
#' }

NULL