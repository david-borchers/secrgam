# #-----------------------------Function--------------------------------------------
# Boland.addcov2mask=function(mask,plot=TRUE){
#   Blocs=as.matrix(data.frame(x=mask$x,y=mask$y))  # locations on mask
#   
#   # Landuse
# #  data(Boland.landuse.image) # get landuse data
#   # interp.surface() is a function in library(fields):
#   Blu=interp.surface(Boland.landuse.image, Blocs) # landuse values at locations
#   Blu.frame=data.frame(x=Blocs[,1],y=Blocs[,2],landuse=round(Blu)) # mask points with landuse
#   water=which(Blu.frame$landuse==5)
#   cpd=which(Blu.frame$landuse==2 | Blu.frame$landuse==3 | Blu.frame$landuse==6) # farmland
#   urban=which(Blu.frame$landuse==4)
#   locs=Blu.frame[,1:2]
#   dists=as.matrix(dist(locs))
#   n=dim(locs)[1]
#   d2h2o=d2cpd=d2urban=rep(NA,n)
#   for(i in 1:n){
#     d2h2o[i]=min(dists[i,water]) # distance to closest water
#     d2cpd[i]=min(dists[i,cpd]) # distance to closest farmland
#     d2urban[i]=min(dists[i,urban]) # distance to closest urban area
#   }
#   Blu.frame$d2h2o=d2h2o
#   Blu.frame$d2cpd=d2cpd
#   Blu.frame$d2urban=d2urban
# 
# #  # altitude
# ##  data(Boland.alt.image) # get altitude data
# #  Balt=interp.surface(Boland.alt.image, Blocs) # altitudes at locations
# #  Balt.frame=data.frame(x=Blocs[,1],y=Blocs[,2],alt=round(Balt)) # mask points with altitude
# #
#   # add to mask
#   newmask=add.mask.covars(Blu.frame,mask,resp=c("landuse","d2h2o","d2urban","d2cpd")) # add landuse covarites to mask
# #  newmask=add.mask.covars(Balt.frame,newmask,resp=c("alt")) # add altitudes to mask
#   
#   if(plot) {
#     covs=names(attr(newmask,"covariates"))
#     ncovs=length(covs)
#     for(i in 1:length(covs)) {
#       dat=data.frame(x=newmask$x, y=newmask$y, z=attr(newmask,"covariates")[,i])
#       cov.image=prep4image(dat)
#       image(cov.image,xlab="Latitude",ylab="Longitude",main=covs[i])
#     }
#   }
#   return(newmask)
# }
# 
# #---------------------------------------------------------------------------------

library(secrgam)

data(Boland.leopards)
names(attr(Boland.mask,"covariates"))

data(Boland.landuse.image)
str(Boland.landuse.image)


# load("~/Packages/secrgam/development/Boland.alt.Rda")

# newmask = Boland.addcov2mask(Boland.mask) # creates mask with landuse (and covariates derived from landuse)

# Landuse definitions in order they appear on website
lu.names=c("Natural","Cultivated","Degraded","Urban","Water","Plantation")
lu.cols=c("green3","yellow","tan","dimgray","blue","gold2")

# Plot landuse
windows()
par(mfrow = c(1,1), mar = c(1,1,1,1), oma = c(0,0,0,0))
image(Boland.landuse.image, col = lu.cols, asp = 1)
points(traps(Boland.CH), pch = "+", col = "red", cex = 1)
legend("topright", legend = lu.names, fill = lu.cols, cex = 0.75, bg = "white")

# Plots with derived landuse covariates at resolution of altitude data:
Blocs=as.matrix(data.frame(x=Boland.alt$x,y=Boland.alt$y))
Blu=interp.surface(lu.image, Blocs)
Blu.frame=data.frame(x=Blocs[,1],y=Blocs[,2],z=round(Blu))
#Blu.image=prep4image(Blu.frame)
#image(Blu.image,col=terrain.colors(30))
#contour(Blu.image,add=TRUE)
#points(traps(Boland.CH),pch=10,col="black",cex=1)
plot(x=Blu.frame$x,y=Blu.frame$y,col=landcols[round(Blu.frame$z)],pch=15,cex=1.6)
points(traps(Boland.CH),pch=10,col="black",cex=1)
legend("topright",legend=landuse[1:6],fill=landcols[1:6],cex=0.75,bg="white")

# Get derived variables on resolution of altitude data
water=which(Blu.frame$z==5)
cpd=which(Blu.frame$z==2 | Blu.frame$z==3 | Blu.frame$z==6)
urban=which(Blu.frame$z==4)
locs=Blu.frame[,1:2]
dists=as.matrix(dist(locs))
n=dim(locs)[1]
d2h2o=d2cpd=d2urban=rep(NA,n)
for(i in 1:n){
  d2h2o[i]=min(dists[i,water])
  d2cpd[i]=min(dists[i,cpd])
  d2urban[i]=min(dists[i,urban])
}
Blu.frame$d2h2o=d2h2o
Blu.frame$d2cpd=d2cpd
Blu.frame$d2urban=d2urban
Blu.d2h2o.image=prep4image(Blu.frame,resp="d2h2o")
Blu.d2urban.image=prep4image(Blu.frame,resp="d2urban")
Blu.d2cpd.image=prep4image(Blu.frame,resp="d2cpd")

# Do the plots:
# Dist to urban map
image(Blu.d2urban.image,xlab="Lat",ylab="Lon",main="Distance to town")
points(traps(Boland.CH),pch=15,col="black",cex=1)
points(Boland.mask,pch=".",cex=2)
#contour(Blu.d2urban.image,add=TRUE)
# Dist to water map
image(Blu.d2h2o.image,xlab="Lat",ylab="Lon",main="Distance to water",col=topo.colors(30))
points(traps(Boland.CH),pch=15,col="black",cex=1)
points(Boland.mask,pch=".",cex=2)
#contour(Blu.d2h2o.image,add=TRUE)
# Dist to farmland map
image(Blu.d2cpd.image,xlab="Lat",ylab="Lon",main="Distance to farmland",col=terrain.colors(30))
points(traps(Boland.CH),pch=15,col="black",cex=1)
points(Boland.mask,pch=".",cex=2)
#contour(Blu.d2cpd.image,add=TRUE)


# Plots to look at spatial covariates on mask
junk=make.mask.image(Blu.mask,"d2urban",plot=TRUE)
points(traps(Boland.CH),pch=15,col="black",cex=1)
contour(junk,add=TRUE)
junk=make.mask.image(Blu.mask,"d2h2o",plot=TRUE)
points(traps(Boland.CH),pch=15,col="black",cex=1)
contour(junk,add=TRUE)
junk=make.mask.image(Blu.mask,"d2cpd",plot=TRUE,col=terrain.colors(30))
points(traps(Boland.CH),pch=15,col="black",cex=1)
contour(junk,add=TRUE)
junk=make.mask.image(Blu.mask,"alt",plot=TRUE,col=terrain.colors(30))
points(traps(Boland.CH),pch=15,col="black",cex=1)
contour(junk,add=TRUE)
