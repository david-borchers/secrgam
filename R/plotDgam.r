#' @title SECR Density spline plotting.
#'   
#' @description Plots regression spline smooth components of an secr density
#' model.
#' 
#' @param fit object returned by \code{\link{secrgam.fit}}.
#' @param type either \code{"link"} (for plot on link funtion scale) or
#'   \code{"response"} (for plot on response scale).
#' @param mask.rug \code{TRUE} if a rugplot of locations of mask points in the
#'   covariate dimension is to be plotted.
#' @param det.rug \code{TRUE} if a rugplot of locations of detectors in the
#'   covariate dimension is to be plotted.
#' @param bounds \code{TRUE} if 95% confidence bounds of the smooth are to be
#' plotted.
#' @param npts number of points on x-axis at which to evaluate smooth.
#' @param main if TRUE, a main heading reflecting the smooth term in the model
#' that is being plotted, is added to the plot; else no main heading is plotted.
#'   
#' @details
#' Plots smooth on link or response scale, together with rug plot showing locations
#' of detectors in covariate space (if det.rug==TRUE), rug plot showing locations
#' of mask points (if mask.rug==TRUE) and 95% confidence bounds (if bounds=TRUE).
#' 
#' Be aware that using mask.rug=TRUE will result in it taking quite a while to 
#' produce the plot
#' @export
#' @examples
#' op = par(no.readonly = TRUE)
#' 
#' data(Boland.fits1)
#' plotDgam(fit1.a3)
#' 
#' par(mfrow = c(1,2))
#' data(Boland.fits2)
#' plotDgam(fit2.a3.dW3)
#' 
#' par(op)

plotDgam = function(fit, type = "response", mask.rug = FALSE, det.rug = TRUE, 
                    bounds=TRUE, npts = 200, main=TRUE){
  
  if(!type %in% c("link", "response")){
    type = "link"
    warning("Invalid type; reset to `link'.")
  }
  
  # get smooth terms and associted variables:
  Dmodel = as.character(fit$Dmodel)
  terms = strsplit(Dmodel, " + ", fixed = TRUE)[[2]]
  sterms = terms[substr(terms, 1, 1) == "s" | substr(terms, 1, 2) == "te"]
  nsp = length(sterms)
  if(nsp == 0) stop("No univariate smooths (s() or te()) in Density model so can't plot anything.")
  svar = rep(NA, nsp)
  for(i in 1:nsp){
    svar[i] = strsplit(strsplit(sterms[i], ", ", fixed = TRUE)[[1]][1], "(", fixed = TRUE)[[1]][2]
  }
  
  # fit$fit$estimate[fit$parindx$D]
  # fit$betanames[fit$parindx$D]
  
  # extract relevant coefficients
  coeff = t(as.vector(coefficients(fit)[1])) 
  cnames = colnames(coeff)
  D.s = which(substr(cnames, 1, 2) == "D.")
  for(i in D.s){ # strip "D." from coeff names
    cnames[i] = substring(cnames[i], 3)
  }
  
  # loop through all smooths:
  for(i in 1:nsp) { # i=1
    # get ith covariate range
    #     if(svar[i] == "x"){
    #       vrange = range(fit$mask$x)
    #     }else if(svar[i] == "y"){
    #       vrange = range(fit$mask$y)
    #     }else {
    vrange = fit$cov.range[, svar[i]]
    #     }
    
    # calculate ith linear predictor over covariate range
    newdata = data.frame(D = rep(1, npts))
    newdata[[svar[i]]] = seq(vrange[1], vrange[2], length = npts) # values to span range of svar[i]
    form = update.formula(as.formula(paste("~", sterms[i], sep = "")), D ~ .) # complete fomula
    
    X = make.density.design.matrix(Dmodel = form, mask = newdata) # don't need nsessions argument here
    
    keepcoeff = c(1, which(is.element(cnames, colnames(X)))) # extract cols for smooth from X (incude intercept)
    
    scoeff = coeff[keepcoeff]
    smooth.x = as.numeric(X %*% scoeff)
    
    if(bounds){
      # Var[Sum(a_i beta_i)] = Sum(a_i * a_j * Cov[beta_i, beta_j])
      # X is matrix of covariates
      # Cov is the covariance matrix for beta
      
      Cov = solve(fit$fit$hessian[keepcoeff,keepcoeff])
      p = ncol(X)
      n = nrow(X)
      se = sapply(1:n, function(i){ # i = 1
        sqrt(sum(sapply(1:p, function(j){ # j = 1
          sapply(1:p, function(k){ # k = 1
            as.numeric(X[i,j] * X[i,k] * Cov[j,k])
          })
        })))
      })
      
      lower = smooth.x - qt(0.975, n-1) * se
      upper = smooth.x + qt(0.975, n-1) * se
      
    }
    
    if(type == "response"){
      smooth.x = exp(smooth.x)
      if(bounds) {
        lower    = exp(lower)
        upper    = exp(upper)
      }
    }
    
    meansm = mean(smooth.x)
    if(bounds) ylim = range(c(smooth.x, lower, upper) - meansm)
    else ylim=range(smooth.x-meansm)
    
    if(main) maintitle=sterms[i]
    else maintitle=""
    plot(newdata[[svar[i]]], smooth.x - meansm, type = "l", ylim=ylim, xlab = svar[i], ylab = paste("Smooth of", svar[i]), main = maintitle)
    
    if(bounds) {
      lines(newdata[[svar[i]]], lower - meansm, lty = 2)
      lines(newdata[[svar[i]]], upper - meansm, lty = 2)
    }
    
    # plot mask rug and detector rug if asked to
    if(mask.rug | det.rug){
      
      # won't work for session level covariates
      if(!svar[i] %in% c("Session","session", colnames(fit$sessioncov))){
        
        # get observed variable values on mask:
        if(svar[i] %in% c("x","y")){
          zvals = fit$mask[[svar[i]]]
          det.zvals = traps(fit$capthist)[[svar[i]]]  
        }else{
          zvals = attr(fit$orig.mask, "covariates")[[svar[i]]]
          det.zvals = trap.covar(traps(fit$capthist), fit$orig.mask, svar[i])
        }
        
        # then add rug:
        if(bounds) rug.y=min(lower-meansm)
        else rug.y=min(smooth.x-meansm)
        if(mask.rug)
          points(zvals, rep(rug.y, length(zvals)), pch = "|", cex = 0.5, col = "gray")
        if(det.rug)
          points(det.zvals, rep(rug.y, length(det.zvals)), pch = "|", cex = 0.75)
        if(bounds) lines(newdata[[svar[i]]], lower-meansm,lty=2) # replot line over rug

      }
    }
  }
}