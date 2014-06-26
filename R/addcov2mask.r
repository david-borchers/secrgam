
#' @title Add covariates to mask from raster data
#'   
#' @description Adds covaraiates to a habitat mask from a GIS landcover data in 
#'   raster format.
#'   
#' @param mask \code{\link{mask}} object (as used by \code{\link{secrgam.fit}} 
#'   and \code{\link{secr.fit}})
#' @param raster list with components x,y, and z in the same style as used by 
#'   contour, persp, image etc. x and y are the X and Y grid values and z is a 
#'   matrix with the corresponding values of the surface (this gets passed to 
#'   \code{\link{interp.surface}})
#' @param names names to associate with the values in \code{raster} (for multiple layer types)
#' @param distance.to names of any layers for which `distance to' covariates are
#'   to be calcuated
#' @param drop names of layers to be deleted from the mask
#' @param plot if TRUE then an image plot will be drawn using 
#'   \code{\link{prep4image}}
#' @param cols colours to associate with the layers (only used if \code{plot =
#'   TRUE})
#' @param domulti logical for categorical integer variable (like land use class) into columns of 
#' binary data for each category at each mask point if TRUE. If FALSE then treats creates a single 
#' column with the integer integer values.
#'   
#' @details If \code{typeof(raster$z) = "double"} then elements of 
#'   \code{raster$z} are assumed to represent a single layer type. If 
#'   \code{typeof(raster$z) = "integer"} then each unique integer is assumed to 
#'   represent a unique landcover type. If the mask already contains covariates
#'   whose names are identical to those supplied in \code{names} then these will
#'   be overwritten.
#' @return Returns a new mask object.
#' @examples
#' # load mask and raster data
#' data(Boland.leopards1)
#' data(Boland.landuse.image)
#' data(Boland.alt.image)
#' 
#' # save par settings
#' op = par(no.readonly = TRUE)
#' 
#' # single landcover type
#' par(mfrow = c(1,1), mar = c(2,2,2,2), oma = c(0,0,0,0))
#' newmask = addcov2mask(Boland.mask1, Boland.alt.image, "altitude")
#' head(attributes(newmask)$covariates)
#' 
#' # mutiple landcover types
#' newmask = addcov2mask(
#'   Boland.mask1, Boland.landuse.image,
#'   names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
#'   cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
#' )
#' head(attributes(newmask)$covariates)
#' 
#' # mutiple landcover types - including distance to water and urbarn
#' par(mfrow = c(1,3))
#' newmask = addcov2mask(
#'   Boland.mask1, Boland.landuse.image,
#'   names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
#'   cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
#'   distance.to = c("Urban","Water")
#' )
#' head(attributes(newmask)$covariates)
#' 
#' # mutiple landcover types - including distance to water and urbarn (and deleting these mask points)
#' newmask = addcov2mask(
#'   Boland.mask, Boland.landuse.image,
#'   names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
#'   cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
#'   distance.to = c("Urban","Water"),
#'   drop = c("Urban","Water")
#' )
#' head(attributes(newmask)$covariates)
#' 
#' # reset par settings
#' par(op)
#' @importFrom fields interp.surface
#' @export

addcov2mask = function(mask, raster, names = NULL, distance.to = NULL, drop = NULL, plot = TRUE, cols = NULL,domulti=FALSE){
  
  if(0){
    data(Boland.leopards); mask = Boland.mask
    data(Boland.landuse.image); raster = Boland.landuse.image
    names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation")
    distance.to = c("Urban","Water")
    drop = c("Urban","Water")
    plot = TRUE
    cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2")
  }
  
  # raster type
  type = switch(typeof(raster$z),
                "double" = "single",
                "integer" = "multi",
                stop("typeof(raster$z) must be integer (i.e. multiple layer types) or numeric (i.e. single layer type)"))
  
  if(type == "single"  & domulti) stop("Can only do multi-layer covariates with integer variables")
  
  if(type == "single"  | !domulti){
    
    # error checks
    if(is.null(names)){
      names = deparse(substitute(raster))
    }else{
      if(length(names) > 1) names = names[1]
    }
    
    # interpolate landcover types onto mask points
    z = round(interp.surface(raster, mask))
    
    # append z variables to mask covariates
#    attributes(mask)$covariates[[names]] = z ; head(attributes(mask)$covariates)
    covariates(mask)[[names]] = z ; head(attributes(mask)$covariates)
    
    if(plot)      
      prep4image(data.frame(mask, z), asp = 1, xlab = "Longitude", ylab = "Latitude", main = names)
    
  }
  
  if(type == "multi" & domulti){
    
    # unique layers in raster data
    layers = sort(unique(as.vector(raster$z))) ; layers
    
    # number of unique layers in raster data
    nlayers = length(layers) ; nlayers
    
    # check layer names
    if(is.null(names)){
      names = paste("newlayer", 1:nlayers, sep = "")
    }else{
      if(length(names) != nlayers) 
        stop("length of names must be equal to the number of layers in raster")
    }
    
    # check layer cols
    if(is.null(cols)){
      cols = terrain.colors(nlayers)
    }else{
      if(length(cols) != nlayers) 
        stop("length of cols must be equal to the number of layers in raster")
    }
    
    # interpolate landcover types onto mask points
    z = round(interp.surface(raster, mask))
    if(any(is.na(z))) {
      z[is.na(z)]=min(raster$z)
      warning("Some interpolated NAs; set these to min(raster$z).")
    }
    
    # convert z to dummy variables
    dummy = as.matrix(model.matrix( ~ as.factor(z)-1, data.frame(mask, z)))
    colnames(dummy) = names[layers %in% sort(unique(z))] ; head(dummy)
    
    # append dummy variables to mask covariates
#    for(i in colnames(dummy)) attributes(mask)$covariates[[i]] = dummy[,i] ; head(attributes(mask)$covariates)
    for(i in colnames(dummy)) covariates(mask)[[i]] = dummy[,i] ; head(attributes(mask)$covariates)
    
    if(!is.null(distance.to)){
      
      # calculate distances to each dist2 layer  
      dist2 = sapply(distance.to, function(layer){
        
        i = dummy[,layer] == 1
        
        # matrix of distances: rows = !layer, cols = layer
        distances = rdist(mask[!i,], mask[i,]) ; dim(distances)
        
        # vector of distances - zero means that the point is within the layer
        dist2 = rep(0, nrow(mask))
        
        # add shortest to layer point from each non-layer point
        dist2[!i] = apply(distances, 1, min)
        
        return(dist2)
        
      })
      
      colnames(dist2) = paste("dist2", distance.to, sep = ".") ; head(dist2) ; dim(dist2)
      
      # append dist2 variables to mask covariates
#      for(i in colnames(dist2)) attributes(mask)$covariates[[i]] = dist2[,i] ; head(attributes(mask)$covariates)
      for(i in colnames(dist2)) covariates(mask)[[i]] = dist2[,i] ; head(attributes(mask)$covariates)
      
    }
    
    if(!is.null(drop)){
      
      for(layer in drop){ # layer = drop[1] ; layer
        
        # which mask points are within this layer
        i = attributes(mask)$covariates[[layer]] == 0

        # delete these rows from the covariates
#        attributes(mask)$covariates = attributes(mask)$covariates[i,]
        covariates(mask) = covariates(mask)[i,]
        
        # delete these elements from z
        z = z[i]
        
        # delete this landcover type from the mask covariates
#        attributes(mask)$covariates[[layer]] = NULL
        covariates(mask)[[layer]] = NULL

        # save mask attributes
        attr = attributes(mask)
        
        # delete these rows from the mask
        mask = mask[i,]
        
        # put back mask attributes
        for(i in c("type", "meanSD", "area", "spacing", "boundingbox", "covariates"))
          attributes(mask)[[i]] = attr[[i]]
        
      }
      
    }
    
    dim(mask) ; length(z)
    dim(attributes(mask)$covariates)
    head(attributes(mask)$covariates)
    
    if(plot){
      
      # landcover layers
      i = layers %in% sort(unique(z))
      prep4image(data.frame(mask, z = as.numeric(as.factor(z))), key = FALSE, contour = FALSE, col = cols[i], asp = 1,
                 xlab = "Latitude", ylab = "Longitude", main = deparse(substitute(raster)))
      
      i = layers %in% sort(unique(z)) & !names %in% drop
      legend("topright", legend = names[i], fill = cols[i], cex = 0.75, bg = "white")
      
      # distance.to layers
      if(!is.null(distance.to))
        for(i in 1:ncol(dist2)) # i = 1
          prep4image(data.frame(mask, z = attr(mask,"covariates")[,colnames(dist2)[i]]),
                     xlab = "Latitude", ylab = "Longitude", main = colnames(dist2)[i], asp = 1)
      
    }
    
  }
  
  return(mask)
  
}

