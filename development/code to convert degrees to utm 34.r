
# code to change coordinates from decimal degrees to UTM 34S 

require(secrgam)

rm(list = ls())

load("~/Packages/secrgam/data/WCape.alt.rda")
load("~/Packages/secrgam/data/WCape.alt.image.rda")

ls()

head(WCape.alt)
str(WCape.alt.image)

# approx conversions factors:

# http://www.csgnetwork.com/degreelenllavcalc.html

# calculated for latitude -33.75

lat2m = 110917.86

lon2m = 92654.52

WCape.alt$x = WCape.alt$x * lon2m
WCape.alt$y = WCape.alt$y * lat2m

WCape.alt.image$x = WCape.alt.image$x * lon2m
WCape.alt.image$y = WCape.alt.image$y * lat2m


if(0){
  
  save(WCape.alt, file = "~/Packages/secrgam/data/WCape.alt.rda")
  save(WCape.alt.image, file = "~/Packages/secrgam/data/WCape.alt.image.rda")
  
}


# 
# 
# # mask -------------------------------------------------------------------
# 
# # save covariates
# covs = attributes(Boland.mask)$covariates
# 
# # get mask coordinates
# mask = as.data.frame(Boland.mask)
# 
# # convert to UTM 34
# # require(rgdal)
# # new = as.data.frame(spTransform(SpatialPoints(as.matrix(Boland.mask), proj4string = CRS("+proj=longlat")), CRS("+proj=utm +zone=34")))
# 
# # approximiate conversion
# mask$x = mask$x * lon2m 
# mask$y = mask$y * lat2m
# 
# # replace old mask
# Boland.mask = read.mask(data = mask)
# 
# # replace covariates
# attributes(Boland.mask)$covariates = covs
# 
# # check area
# attributes(Boland.mask)$area * nrow(Boland.mask) / 100 # 2226.054 sq km
# 
# plot(Boland.mask)
# 
# 
# 
# # traps -------------------------------------------------------------------
# 
# # save traps 
# traps = attributes(Boland.CH)$traps
# 
# # save usage
# usage = attributes(traps)$usage ; usage
# 
# # convert to UTM 34
# # require(rgdal)
# # new = as.data.frame(spTransform(SpatialPoints(as.matrix(traps), proj4string = CRS("+proj=longlat")), CRS("+proj=utm +zone=34")))
# 
# # approximiate conversion
# traps$x = traps$x * lon2m
# traps$y = traps$y * lat2m
# 
# # make new traps object
# traps = read.traps(data = as.data.frame(traps), detector = "proximity")
# 
# # add usage info
# attributes(traps)$usage = usage
# 
# # add new traps objects to capthist
# attributes(Boland.CH)$traps = traps
# 
# # add to plot
# plot(attributes(Boland.CH)$traps, add = TRUE)
# 
# 
# 
# # image -------------------------------------------------------------------
# 
# Boland.image$x = Boland.image$x * lon2m
# Boland.image$y = Boland.image$y * lat2m
# 
# 
# 
# # save --------------------------------------------------------------------
# 
# save(Boland.mask, Boland.CH, Boland.image, file = "~/Packages/secrgam/data/Boland.leopards.rda")








