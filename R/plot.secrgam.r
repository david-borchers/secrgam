
#' @title Plot fitted density surface or detection function
#'   
#' @description Plot the fitted density surface or fitted detection function from an \code{secrgam} model.
#'   
#' @param fit a fitted \code{\link{secrgam}} or \code{\link{secr}} model
#' @param density if \code{TRUE} then the fitted density surface will be plotted, if \code{FALSE} then \code{\link{plot.secr}} is used to plot the fitted detection function
#' @param mask an optional \code{\link{mask}} object to use for plotting the fitted density surface
#' @param ... aditional arguments to pass to \code{prep4image} (if \code{density = TRUE}), or \code{plot.secr} (if \code{density = FALSE})
#' @examples
#' data(Boland.leopards1)
#' data(Boland.fits1)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,2), oma = c(0,0,0,0), mar = c(4,4,4,4))
#' 
#' # density surface
#' plot(fit1.a3, asp = 1, key = FALSE, main = "Density")
#' 
#' # detection function
#' plot(fit1.a3, density = FALSE, xval = 0:20000, main = "Detection function")
#' 
#' par(op)
#' @export
#' @seealso \code{\link{fitted.secrgam}}

plot.secrgam = function(fit, density = TRUE, mask = NULL, ...){
  
  if(density){
    
    # plot fitted density surface
    
    if(is.null(mask)){
      
      prep4image(data.frame(x = fit$mask$x, y = fit$mask$y, z = fitted(fit)), ...)  
      
    }else{
      
      prep4image(data.frame(x = mask$x, y = mask$y, z = fitted(fit, mask)), ...)  
      
    }
    
  }else{
    
    # plot fitted detection function
    secr:::plot.secr(fit, ...)
    
  }
  
}