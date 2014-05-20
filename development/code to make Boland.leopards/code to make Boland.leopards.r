## ************************************************************************** ##
## ************************************************************************** ##
##                                                                            ##
##                    code to make Boland.leopards.rda                        ##
##                                                                            ##
## ************************************************************************** ##
## ************************************************************************** ##

wd = setwd("~/Packages/secrgam/development/code to make Boland.leopards/original data")


# load original data ------------------------------------------------------

load("Boland.leopards.Rda")
load("Boland.landuse.image.Rda")



# conversion factors ------------------------------------------------------

# approx conversions factors calculated for latitude -33.75
# http://www.csgnetwork.com/degreelenllavcalc.html

lat2m = 110917.86
lon2m = 92654.52



# mask -------------------------------------------------------------------

# save covariates
covs = attributes(Boland.mask)$covariates

# get mask coordinates
mask = as.data.frame(Boland.mask)

# approximiate conversion
mask$x = mask$x * lon2m 
mask$y = mask$y * lat2m

# replace old mask
Boland.mask = read.mask(data = mask)

# replace covariates
attributes(Boland.mask)$covariates = covs

# check area
attributes(Boland.mask)$area * nrow(Boland.mask) / 100 # 2226.054 sq km

plot(Boland.mask)



# traps -------------------------------------------------------------------

# save traps 
traps = attributes(Boland.CH)$traps

# save usage
usage = attributes(traps)$usage ; usage

# approximiate conversion
traps$x = traps$x * lon2m
traps$y = traps$y * lat2m

# make new traps object
traps = read.traps(data = as.data.frame(traps), detector = "proximity")

# add usage info
attributes(traps)$usage = usage

# add new traps objects to capthist
attributes(Boland.CH)$traps = traps

# add to plot
plot(attributes(Boland.CH)$traps, add = TRUE)



# image -------------------------------------------------------------------

Boland.image$x = Boland.image$x * lon2m
Boland.image$y = Boland.image$y * lat2m



# landuse image -----------------------------------------------------------

Boland.landuse.image$x = Boland.landuse.image$x * lon2m
Boland.landuse.image$y = Boland.landuse.image$y * lat2m



# save --------------------------------------------------------------------

save(Boland.mask, Boland.CH, Boland.image, file = "~/Packages/secrgam/data/Boland.leopards.rda")
save(Boland.landuse.image, file = "~/Packages/secrgam/data/Boland.landuse.image.rda")



setwd(wd)


## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
