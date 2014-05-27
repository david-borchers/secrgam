#' @title Effective survey area calculation for secrgam objects
#'   
#' @description If the density model is uniform, calls \code{esa} from package 
#' \code{\link{secr}} to calculate effective survey area, else returns an error message.
#'   
#' @param object a fitted secrgam model (returned from \code{\link{secrgam.fit}})
#' @param ... additional arguments passed to \code{esa} for package \code{\link{secr}}.
#' @param use.secr logical which, when TRUE, forces estimation with \code{esa} from 
#' package \code{\link{secr}}.
#'   
#' @details The esa for non-uniform density models is not yet implemented. See \code{esa}
#' in package \code{\link{secr}} for details of esa calculation when density is uniform.
#' @export
esa=function(object,...,use.secr=FALSE){
  if(use.secr){
    return(secr:::esa(object,...))
  } else {
    if(inherits(object,"secrgam")) {
      if(((object$Dmodel=="~-1") | (object$Dmodel=="~ -1") |
            (object$Dmodel=="D~-1") | (object$Dmodel=="D ~ -1"))) {
        class(object="secr")
        return(secr:::esa(object,...))
      } else stop("Function esa is not implemented for non-uniform density surfaces.")
    } else if(inherits(object,"secrgam")) return(secr:::esa(object,...))
  }
}