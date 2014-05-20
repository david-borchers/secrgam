
#' @title Add covariates to mask from raster data
#'   
#' @description Adds covaraiates to a habitat mask from a GIS landuse data in
#'   raster format.
#'   
#' @param mask description needed...
#' @param raster description needed...
#' @param layer.names description needed...
#' @param distance.to.layer description needed...
#' @param plot description needed...
#' @param layer.cols description needed...
#' @param ... additional arguments to pass to \code{\link{prep4image}}
#'   
#' @details details needed...
#' @examples
#' # load mask and raster data
#' data(Boland.leopards)
#' data(Boland.landuse.image)
#' 
#' # function defaults
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,1), mar = c(2,2,2,2), oma = c(0,0,0,0))
#' newmask = addcov2mask(
#'   Boland.mask, Boland.landuse.image,
#'   layer.names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
#'   layer.cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
#' )
#' par(op)
#' head(attributes(newmask)$covariates)
#' 
#' # specify colors and calculate distance to water and urbarn landclasses
#' op = par(no.readonly = TRUE)
#' par(mfrow = c(1,3), mar = c(2,2,2,2), oma = c(0,0,0,0))
#' newmask = addcov2mask(
#'   Boland.mask, Boland.landuse.image,
#'   layer.names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
#'   layer.cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
#'   distance.to.layer = c("Urban","Water")
#' )
#' par(op)
#' head(attributes(newmask)$covariates)
#' @importFrom fields interp.surface
#' @export

addcov2mask = function(mask, raster, layer.names = NULL, distance.to.layer = NULL, plot = TRUE, layer.cols = NULL){
  
  # number of mask points
  M = nrow(mask) ; M
  
  # unique layers in raster data
  layers = sort(unique(as.vector(raster$z))) ; layers
  nlayers = length(layers) ; nlayers
  
  # check length of layer names and cols
  if(is.null(layer.names)){
    layer.names = paste("layer", 1:nlayers, sep = "")
  }else{
    if(length(layer.names) != nlayers) 
      stop("length of layer.names must be equal to the number of layers in raster")
  }
  if(is.null(layer.cols)){
    layer.cols = terrain.colors(nlayers)
  }else{
    if(length(layer.cols) != nlayers) 
      stop("length of layer.cols must be equal to the number of layers in raster")
  }
  
  # interpolate landuse types onto mask points
  z = round(interp.surface(raster, mask))
  
  # check if any layers dropped
  keep = layers %in% sort(unique(z))
  layers = layers[keep] ; layers
  nlayers = length(layers) ; nlayers
  layer.names = layer.names[keep] ; layer.names
  layer.cols = layer.cols[keep] ; layer.cols
  
  # convert to dummy variables
  dummy = as.matrix(model.matrix( ~ as.factor(z)-1, data.frame(mask, z)))
  colnames(dummy) = layer.names ; head(dummy)
  
  # append dummy variables to mask covariates
  attributes(mask)$covariates = cbind(attributes(mask)$covariates, dummy) ; head(attributes(mask)$covariates)
  
  if(!is.null(distance.to.layer)){
    
    # calculate distances to each dist2 layer  
    dist2 = sapply(distance.to.layer, function(layer){
      
      i = dummy[,layer] == 1
      
      # matrix of distances: rows = !layer, cols = layer
      distances = rdist(mask[!i,], mask[i,]) ; dim(distances)
      
      # vector of distances
      dist2 = rep(NA, M)
      
      # add shortest to layer point from each non-layer point
      dist2[!i] = apply(distances, 1, min)
      
      return(dist2)
      
    })
    
    colnames(dist2) = paste("dist2", distance.to.layer, sep = ".") ; head(dist2) ; dim(dist2)
    
    # delete dist2 layers in dummy
    dummy = dummy[, !colnames(dummy) %in% distance.to.layer] ; head(dummy) ; dim(dummy)
    
    # remove dist2 layers from dummy covariates
    for(i in distance.to.layer) attributes(mask)$covariates[[i]] = NULL ; head(attributes(mask)$covariates)
    
    # append dist2 variables to mask covariates
    attributes(mask)$covariates = cbind(attributes(mask)$covariates, dist2) ; head(attributes(mask)$covariates)
    
    # delete mask points in dist2 layers
    keep = z %in% layers[!layer.names %in% distance.to.layer]
    z = z[keep]
    attributes(mask)$covariates = attributes(mask)$covariates[keep,]
    attr = attributes(mask)
    mask = mask[keep,] ; dim(mask)
    for(i in c("type", "meanSD", "area", "spacing", "boundingbox", "covariates"))
      attributes(mask)[[i]] = attr[[i]]
    dim(attributes(mask)$covariates)
    
  }
  
  # individual plots of mask covariates
  if(plot){
    
    # par(mfrow(1,3))

    # coveriate layers
    i = !layer.names %in% distance.to.layer ; i
    prep4image(data.frame(mask, z), key = FALSE, contour = FALSE,
               xlab = "Latitude", ylab = "Longitude", main = deparse(substitute(raster)), col = layer.cols, asp = 1)
    legend("topright", legend = layer.names[i], fill = layer.cols[i], cex = 0.75, bg = "white")

    # distance to non-covariate layers
    if(!is.null(distance.to.layer))
      for(i in 1:ncol(dist2)) # i = 1
        prep4image(data.frame(mask, z = attr(mask,"covariates")[,colnames(dist2)[i]]),
                   xlab = "Latitude", ylab = "Longitude", main = colnames(dist2)[i], asp = 1)
    
  }
  
  return(mask)

}

