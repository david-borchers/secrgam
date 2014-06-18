#' @title SECR Density spline plotting.
#'
#' @description
#'  Plots B-spline smooth components of an secr density model
#'  
#' @param fit object returned by secr.fit().
#' @param type either "link" (for plot on link funtion scale) or "response" (for plot on
#' response scale).
#' @param mask.rug TRUE if a rugplot of locations of mask points in the covariate dimension
#' is to be plotted.
#' @param det.rug TRUE if a rugplot of locations of detectors in the covariate dimension
#' is to be plotted.
#' @param npts number of points on x-axis at which to evaluate smooth.
#' @param show.knots if TRUE, puts crosses on smooth at knot locations. NOT YET IMPLEMENTED.
#' 
#' @details
#' (None as yet)
#' @export

plotDgam = function(fit, type = "link", mask.rug = FALSE, det.rug = TRUE, npts = 200, show.knots = FALSE){

  if(type != "link" & type != "response") {
    type = "link"
    warning("Invalid type; reset to `link'.")
  }
  if(show.knots) warning("Knot plotting not yet implemented"
                         )
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

  # extract relevant coefficients
  coeff = t(as.vector(coefficients(fit)[1])) 
  cnames = colnames(coeff)
  D.s = which(substr(cnames, 1, 2) == "D.")
  for(i in D.s){ # strip "D." from coeff names
    cnames[i] = substring(cnames[i], 3)
  }
  
  # loop through all smooths:
  for(i in 1:nsp) {
    # get ith covariate range
    if(svar[i] == "x"){
      vrange = range(fit$mask$x)
    }else if(svar[i] == "y"){
      vrange = range(fit$mask$y)
    }else {
      vrange = fit$cov.range[, svar[i]]
    }
    
    # calculate ith linear predictor over covariate range
    mask = data.frame(D = rep(1, npts), v = seq(vrange[1], vrange[2], length = npts)) # values to span range of svar[i]
    names(mask)[2] = svar[i]
    form = update.formula(as.formula(paste("~", sterms[i], sep = "")), D ~ .) # complete fomula
    G = gam(formula = form, data = mask, fit = FALSE)  
    X = G$X # extract the design matrix
    colnames(X) = G$term.names # add the column names
    # clean up the names so they can be used inside formula objects (without the use of backticks)
    colnames(X) = gsub("\\(", ".", colnames(X)) 
    colnames(X) = gsub("[\\)]|[, ]", "", colnames(X))
    keepcoeff = c(1, which(is.element(cnames, colnames(X)))) # extract cols for smooth from X (incude intercept)
    scoeff = coeff[keepcoeff]
    lp = X%*%scoeff
    if(type == "response") smooth.x = exp(as.numeric(lp))
    else smooth.x = as.numeric(lp)
    meansm = mean(smooth.x)
    plot(mask[, 2], smooth.x-meansm, type = "l", ylim = range(smooth.x-mean(smooth.x)), xlab = svar[i], ylab = paste("Smooth of", svar[i]), main = sterms[i])

    # plot mask rug and detector rug if asked to
    if(mask.rug | det.rug){
      # get observed variable values on mask:
      if(svar[i] == "x" | svar[i] == "y") {
        zvals = fit$mask[[svar[i]]]
        det.zvals = traps(fit$capthist)[[svar[i]]]
      } else {
        zvals = attr(fit$orig.mask, "covariates")[[svar[i]]]
        det.zvals = trap.covar(traps(fit$capthist), fit$orig.mask, svar[i])
      }
      # then add rug:
      if(mask.rug)
        points(zvals, rep(min(smooth.x-meansm), length(zvals)), pch = "|", cex = 0.5, col = "gray")
      if(det.rug)
        points(det.zvals, rep(min(smooth.x-meansm), length(det.zvals)), pch = "|", cex = 0.75)
      lines(mask[, 2], smooth.x-meansm) # replot line over rug
    }
  }
}