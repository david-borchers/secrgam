#' @title Derived density calculation for secrgam objects
#'   
#' @description If the density model is uniform, calls \code{derived} from package 
#' \code{\link{secr}} to calculate density, else returns an error message.
#'   
#' @param object a fitted secrgam model (returned from \code{\link{secrgam.fit}})
#' @param ... additional arguments passed to \code{derived} for package \code{\link{secr}}.
#' @param use.secr logical which, when TRUE, forces estimation with \code{derived} from 
#' package \code{\link{secr}}.
#'   
#' @details The density for non-uniform density models is not yet implemented. 
#' See \code{derived} in package \code{\link{secr}} for details of derived calculations 
#' when density is uniform.
#' @export
derived=function(object,...,use.secr=FALSE){
  if(use.secr){
    return(secr:::derived(object,...))
  } else {
    if(inherits(object,"secrgam")) {
      if(((object$Dmodel=="~-1") | (object$Dmodel=="~ -1") |
            (object$Dmodel=="D~-1") | (object$Dmodel=="D ~ -1"))) {
        class(object="secr")
        return(secr:::derived(object,...))
      } else stop("Function derived is not implemented for non-uniform density surfaces.")
    } else if(inherits(object,"secrgam")) return(secr:::derived(object,...))
  }
}