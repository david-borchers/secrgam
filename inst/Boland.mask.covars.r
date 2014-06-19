# load mask and raster data
data(Boland.leopards)
data(Boland.landuse.image)
data(Boland.alt.image)

# save par settings
op = par(no.readonly = TRUE)

# single landcover type
par(mfrow = c(1,1), mar = c(2,2,2,2), oma = c(0,0,0,0))
newmask = addcov2mask(Boland.mask, Boland.alt.image, "altitude")
head(attributes(newmask)$covariates)

par(mfrow = c(2,2))
# numeric landcover
newmask = addcov2mask(Boland.mask, Boland.landuse.image,names = c("Landuse"))
# mutiple landcover types - including distance to water and urbarn
LUnames=c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation")

newmask = addcov2mask(newmask, Boland.landuse.image,
                      names = LUnames, 
                      cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
                      distance.to = c("Urban","Water","Natural"),domulti=TRUE
)
LUfactor=LUnames[covariates(newmask)$Landuse]
covariates(newmask)=cbind(covariates(newmask),LUfactor=as.factor(LUfactor))
head(attributes(newmask)$covariates)
Boland.mask=newmask
keep=covariates(Boland.mask)$Landuse!=4 & covariates(Boland.mask)$Landuse!=5 # remove water and urban
Boland.mask=Boland.mask[keep,]
covariates(Boland.mask)=covariates(Boland.mask)[keep,]
covariates(Boland.mask)=covariates(Boland.mask)[,-c(2,3,6:10)]
save(Boland.mask,file="Boland.mask.rda")








# stuff below here may once have been used - but was not for example

# mutiple landcover types
newmask = addcov2mask(
  Boland.mask, Boland.landuse.image,
  names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
  cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
)
head(attributes(newmask)$covariates)

# mutiple landcover types - including distance to water and urbarn
par(mfrow = c(1,3))
newmask = addcov2mask(
  Boland.mask, Boland.landuse.image,
  names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
  cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
  distance.to = c("Urban","Water")
)
head(attributes(newmask)$covariates)

# mutiple landcover types - including distance to water and urbarn (and deleting these mask points)
newmask = addcov2mask(
  Boland.mask, Boland.landuse.image,
  names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
  cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
  distance.to = c("Urban","Water"),
  drop = c("Urban","Water")
)
head(attributes(newmask)$covariates)
