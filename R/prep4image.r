
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
#' @param ... other arguments to pass to \code{\link{image.plot}} (only used 
#'   when \code{plot=TRUE})
#'   
#' @details Sorts z on values of x first, then y, then creates a matrix of 
#'   z-values from this. Returns a list with elements x (unique values of x, in 
#'   increasing order), y (unique values of y, in increasing order) and z 
#'   (matrix of z-values in appropriate order for image/contour/persp).
#' @export
#' @importFrom fields image.plot

prep4image = function(data, plot = TRUE, contour = TRUE, ...){
  
  x = sort(unique(data$x))
  y = sort(unique(data$y))

  z = matrix(NA, nr = length(x), nc = length(y))
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      m = which(data$x == x[i] & data$y == y[j]) ; m
      z[i,j] = if(length(m) == 0) NA else data$z[m]
    }
  }
  
  if(plot){
    image.plot(x, y, z, ...)
    if(contour) contour(x, y, z, add = TRUE)
  }
  
  invisible(list(x = x, y = y, z = z))
  
}