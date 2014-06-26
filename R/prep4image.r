
#' @title Prepares data frame for plotting with image/contour/persp.
#'   
#' @description From an input data frame with columns x, y and z, this function 
#'   creates a list with elements x, y and z in a format suitable for passing to
#'   functions \code{\link{image}}, \code{\link{contour}} or 
#'   \code{\link{persp}}. The coordinates in \code{data} are assumed to come
#'   from a 2D grid of points.
#'   
#' @param data a data frame with columns x and y being Cartesian coordinates, 
#'   and z being the values of some variable at each coordinate.
#' @param plot if \code{TRUE} then an image plot will be drawn using 
#'   \code{\link{image.plot}}
#' @param contour if \code{TRUE} then contours will be added if (only used when 
#'   \code{plot=TRUE})
#' @param key logical for whether or not to include key when \code{plot = TRUE} (\code{\link{image.plot}} is used when \code{key = TRUE}, \code{\link{image}} is used when \code{key = FALSE})
#' @param ... other arguments to pass to \code{\link{image}} or \code{\link{image.plot}} (only used 
#'   when \code{plot=TRUE})
#'   
#' @details Sorts z on values of x first, then y, then creates a matrix of 
#'   z-values from this. Returns a list with elements x (unique values of x, in 
#'   increasing order), y (unique values of y, in increasing order) and z 
#'   (matrix of z-values in appropriate order for image/contour/persp).
#' @export
#' @importFrom fields image.plot
#' @examples
#' data(Boland.alt)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,1))
#' 
#' # make object for use with image
#' imagedata = prep4image(Boland.alt, plot = FALSE)
#' image(imagedata, asp = 1)
#' contour(imagedata, add = TRUE)
#' 
#' # use directly for plotting
#' prep4image(Boland.alt, asp = 1)
#' prep4image(Boland.alt, asp = 1, contour = FALSE)
#' prep4image(Boland.alt, asp = 1, key = FALSE)
#' prep4image(Boland.alt, asp = 1, contour = FALSE, key = FALSE)
#' 
#' par(op)

prep4image = function(data, plot = TRUE, contour = TRUE, key = TRUE, ...){
  
  data = as.matrix(data)
  
  x = sort(unique(data[,"x"]))
  y = sort(unique(data[,"y"]))
  
  z = matrix(NA, nr = length(x), nc = length(y))
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      m = which(data[,"x"] == x[i] & data[,"y"] == y[j]) ; m
      z[i,j] = if(length(m) == 0) NA else data[,"z"][m]
    }
  }
  
  if(plot){
    if(key){
      image.plot(x, y, z, ...)
    }else{
      image(x, y, z, ...)
    }
    if(contour) contour(x, y, z, add = TRUE)
  }
  
  invisible(list(x = x, y = y, z = z))
  
}