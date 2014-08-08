
#' @title Plot fitted density surface or detection function
#'   
#' @description Plot the fitted density surface or fitted detection function from an \code{secrgam} model.
#'   
#' @param fit a fitted \code{\link{secrgam}} or \code{\link{secr}} model
#' @param type the type of plot to produce (see Details)
#' @param mask an optional \code{\link{mask}} object to use when \code{type = "density"}
#' @param ... aditional arguments to pass to the relevant ploting function
#' @details \tabular{ll}{ 
#' type      \tab plotting function \cr 
#' "default" \tab \code{\link{plot.secr}} \cr 
#' "density" \tab \code{\link{prep4image}} \cr 
#' "smooth"  \tab \code{\link{plotDgam}} \cr 
#' 
#' }
#' @examples
#' data(Boland.leopards1)
#' data(Boland.fits1)
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,2), oma = c(0,0,0,0), mar = c(4,4,4,4))
#' 
#' # density surface
#' plot(fit1.a3.dW3,type="density",asp=1,key=FALSE,col=tim.colors(30))
#' # plot detection function
#' plot(fit1.a3.dW3,xval=seq(0,11000,length=100), main = "Density")
#' 
#' # plot smooths
#' plot(fit1.a3.dW3,type="smooth")
#' 
#' par(op)
#' @export
#' @seealso \code{\link{fitted.secrgam}}

plot.secrgam = function(fit, type = c("default", "density", "smooth"), mask = NULL, ...){
  
  type = match.arg(type)
  
  if(type == "default"){
    
    secr:::plot.secr(fit, ...)
    
  }
  
  # scaled smooths
  if(type == "smooth"){
    
    plotDgam(fit, ...)
    
  }
  
  # plot fitted density surface
  if(type == "density"){
    
    if(ms(fit$capthist)) 
      stop("plotting methods for type = 'density' not yet implemented for multi-session data")
    
    if(is.null(mask)){
      
      prep4image(data.frame(x = fit$mask$x, y = fit$mask$y, z = fitted(fit)), ...)  
      
    }else{
      
      prep4image(data.frame(x = mask$x, y = mask$y, z = fitted(fit, mask)), ...)  
      
    }
    
  }
  
}