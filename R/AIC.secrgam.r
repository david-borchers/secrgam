#' @title Compare SECRgam models.
#'   
#' @description Terse report on the fit of one or more spatially explicit capture–recapture models. 
#' Models with smaller values of AIC (Akaike's Information Criterion) are preferred.
#'   
#' @param object secr object output from the function secr.fit, secrgam.fit, or a list of such 
#' objects with class c("list","secrlist").
#' @param ...  other secr objects
#' @param sort  logical for whether rows should be sorted by ascending AICc
#' @param k numeric, penalty per parameter to be used; always k = 2 in this method
#' @param dmax	numeric, maximum AIC difference for inclusion in confidence set
#' @param criterion	character, criterion to use for model comparison and weights.
#' 
#' @details Same as the function of same name in package \code{secr}, but also takes objects of
#' class \code{secrgam} as input. See \code{help(AIC.secr)} for details.
#' 
#' @details NOTE: This function can be used to compare objects of class "secrgam" (output by secrgam.fit)
#' and those of class "secr" PROVIDING that the first object passed to the function is of class "secrgam". 
#' 
#' @export
AIC.secrgam=function(object,...,sort=TRUE,k=2,dmax=10,criterion=c("AICc","AIC"))
{
  allargs <- list(...)
  nargs=length(allargs)
  if(nargs>0)
    for(i in 1:nargs) if(is.element("secrgam",class(allargs[[i]]))) class(allargs[[i]])="secr"
  if(is.element("secrgam",class(object))) class(object)="secr"
  modelnames <- (c(as.character(match.call(expand.dots = FALSE)$object), 
                   as.character(match.call(expand.dots = FALSE)$...)))
  allargs <- secrlist(object, allargs)
  names(allargs) <- modelnames
  AIC(allargs, sort = sort, k = k, dmax = dmax, criterion = criterion)
}


#' @title Prints AIC table using AIC.secr output as input.
#'   
#' @description Same as the function of same name in package \code{secr}, but with explicit 
#' call to secr:::oneline.secr rather than just onleline.secr.
#'   
#' @param object secr object output from the function secr.fit, or a list of such objects with 
#' class c("list","secrlist").
#' @param ...	other secr objects
#' @param sort	logical for whether rows should be sorted by ascending AICc
#' @param k numeric, penalty per parameter to be used; always k = 2 in this method
#' @param dmax	numeric, maximum AIC difference for inclusion in confidence set
#' @param criterion	character, criterion to use for model comparison and weights.
#' 
#' @details see \code{AIC.secr}
#' @export
AIC.secrlist=function(object,...,sort=TRUE,k=2,dmax=10,criterion=c("AICc","AIC")) 
{
  if (k != 2) 
    stop("AIC.secr defined only for k = 2")
  if (length(list(...)) > 0) 
    warning("... argument ignored in 'AIC.secrlist'")
  if (length(object) > 1) {
    hcovs <- sapply(object, function(x) if (is.null(x$hcov)) 
      ""
      else x$hcov)
    if (length(unique(hcovs)) > 1) 
      stop("AIC invalid when models use different hcov")
  }
  criterion <- match.arg(criterion)
  modelnames <- names(object)
  allargs <- object
  if (any(sapply(allargs, class) != "secr")) 
    stop("components of 'object' must be 'secr' objects")
  output <- data.frame(t(sapply(allargs, secr:::oneline.secr)), stringsAsFactors = F)
  for (i in 3:6) output[, i] <- as.numeric(output[, i])
  output$delta <- output[, criterion] - min(output[, criterion])
  OK <- abs(output$delta) < abs(dmax)
  sumdelta <- sum(exp(-output$delta[OK]/2))
  output$wt <- ifelse(OK, round(exp(-output$delta/2)/sumdelta, 
                                4), 0)
  row.names(output) <- modelnames
  if (sort) 
    output <- output[order(output[, criterion]), ]
  names(output)[7] <- paste("d", criterion, sep = "")
  names(output)[8] <- paste(criterion, "wt", sep = "")
  if (nrow(output) == 1) {
    output[, 8] <- NULL
    output[, 7] <- NULL
  }
  output
}
