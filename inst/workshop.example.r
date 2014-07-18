library(secrgam)
data(Boland.leopards1)
#load("Boland.mask.rda") # revised mask to include landuse and related covariates

cameras=traps(Boland.CH1)
image.plot.mask(Boland.mask1,covariate="alt",asp=1,col=terrain.colors(30))
plot(cameras,add=TRUE,detpar=list(pch="+",cex=1.2,col="black"))
image.plot.mask(Boland.mask1,covariate="dist2.Urban",asp=1)
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask1,covariate="dist2.Water",asp=1)
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask1,covariate="LUfactor",asp=1,
                col=c("black","yellow","green3","blue"))
plot(cameras,add=TRUE)
#'   names = c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation"), 
#'   cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),

model.ex = list(D ~ s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) # smooth of altitude with 3 degrees of freedom
system.time(fit.ex <- secrgam.fit(capthist = Boland.CH, model = model.ex, mask = Boland.mask, trace = FALSE))
fit.ex <- secrgam.fit(capthist = Boland.CH, model = model.ex, mask = Boland.mask, trace = TRUE)
fit.ex # look at fit results
plot(fit.ex, asp = 1)
points(traps(Boland.CH), col="black",pch=19)
plotDgam(fit.ex)

# Create a much larger trap array:
cameras2=make.grid(nx=10,ny=11,spacing=4000)
cameras2$x=cameras2$x+1760000
cameras2$y=cameras2$y-3760000
image.plot(Boland.image)
points(cameras2,pch="+")

newmask=make.mask(cameras2,buffer=9000,nx=80,ny=80,type="trapbuffer")
image.plot(Boland.image)
points(newmask,pch=".")
# add altitude
mask2 = addcov2mask(newmask, Boland.image, "alt")
head(covariates(mask2))
points(cameras2,pch="+")
# add landuse covariates
# numeric landcover
newmask = addcov2mask(mask2, Boland.landuse.image,names = c("Landuse"))
# mutiple landcover types - including distance to water and urbarn
LUnames=c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation")

newmask = addcov2mask(newmask, Boland.landuse.image,
                      names = LUnames, 
                      cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
                      distance.to = c("Urban","Water","Natural"),domulti=TRUE
)
LUfactor=LUnames[covariates(newmask)$Landuse+1]
covariates(newmask)=cbind(covariates(newmask),LUfactor=as.factor(LUfactor))
head(attributes(newmask)$covariates)
Boland.mask2=newmask
covariates(Boland.mask2)=covariates(Boland.mask2)[,-c(3,5:9)] # remove some cols don't want
# attach mask covariates to traps
camcov=addCovariates(cameras2,Boland.mask2)
# remove cameras in water and urban
keepcam=covariates(camcov)$Landuse!=4 & covariates(camcov)$Landuse!=5 # remove water and urban
cameras2=camcov[keepcam,]
covariates(cameras2)=covariates(cameras2)[keepcam,]
# remove mask points in water and urban
keep=covariates(Boland.mask2)$Landuse!=4 & covariates(Boland.mask2)$Landuse!=5 # remove water and urban
Boland.mask2=Boland.mask2[keep,]
covariates(Boland.mask2)=covariates(Boland.mask2)[keep,]

# Now recreate mask for cameras2 (having removed cameras in urban and water areas)
newmask=make.mask(cameras2,buffer=9000,nx=80,ny=80,type="trapbuffer")
image.plot(Boland.image)
points(newmask,pch=".")
# add altitude
mask2 = addcov2mask(newmask, Boland.image, "alt")
head(covariates(mask2))
points(cameras2,pch="+")
# add landuse covariates
# numeric landcover
newmask = addcov2mask(mask2, Boland.landuse.image,names = c("Landuse"))
# mutiple landcover types - including distance to water and urbarn
LUnames=c("Unknown", "Natural", "Cultivated", "Degraded", "Urban", "Water", "Plantation")

newmask = addcov2mask(newmask, Boland.landuse.image,
                      names = LUnames, 
                      cols = c("black", "green3", "yellow", "tan", "dimgray", "blue", "gold2"),
                      distance.to = c("Urban","Water","Natural"),domulti=TRUE
)
LUfactor=LUnames[covariates(newmask)$Landuse+1]
covariates(newmask)=cbind(covariates(newmask),LUfactor=as.factor(LUfactor))
head(attributes(newmask)$covariates)
Boland.mask2=newmask
covariates(Boland.mask2)=covariates(Boland.mask2)[,-c(3,5:9)] # remove some cols don't want
# remove mask points in water and urban
keep=covariates(Boland.mask2)$Landuse!=4 & covariates(Boland.mask2)$Landuse!=5 # remove water and urban
Boland.mask2=Boland.mask2[keep,]
covariates(Boland.mask2)=covariates(Boland.mask2)[keep,]

image.plot.mask(Boland.mask2,covariate="alt",asp=1); points(cameras2,pch="+")
image.plot.mask(Boland.mask2,covariate="dist2.Urban",asp=1); points(cameras2,pch="+")
image.plot.mask(Boland.mask2,covariate="dist2.Water",asp=1); points(cameras2,pch="+")


# use a fitted model
popn = sim.popn.secrgam(fit=fit.ex,N=150,mask=Boland.mask2)
head(popn)
dim(popn)
Boland.CH2=sim.capthist(cameras2,popn,noccasions=13,detectpar=list(g0=0.19,sigma=2500),seed=123457)
plot(Boland.CH2,border=0,rad=500,tracks=TRUE,icolour=colors()[seq(2,202,10)],gridlines=FALSE)

model.N.a3.dW3 = list(D ~ Natural+s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) 
system.time(fit2.N.a3.dW3<-secrgam.fit(capthist=Boland.CH2,model=model.N.a3.dW3,mask=Boland.mask2,trace=FALSE))

model.a3.dW3 = list(D ~ s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) 
system.time(fit2.a3.dW3<-secrgam.fit(capthist=Boland.CH2,model=model.a3.dW3,mask=Boland.mask2,trace=FALSE))

fit2.0<-secrgam.fit(capthist=Boland.CH2,model=list(D~1),mask=Boland.mask2,trace=FALSE)

AIC(fit2.N.a3.dW3,fit2.a3.dW3,fit2.0)

plot(fit2.a3.dW3, asp = 1)
points(cameras2, col="black",pch="+")
region.N(fit2.a3.dW3)
region.N(fit2.0)
plotDgam(fit2.a3.dW3)


# Repeat for the original design:
Boland.CH1=sim.capthist(cameras,popn,noccasions=13,detectpar=list(g0=0.19,sigma=2500),seed=123457)
plot(Boland.CH1,border=0,rad=500,tracks=TRUE,icolour=colors()[seq(2,202,10)],gridlines=FALSE)
Boland.mask1=Boland.mask
save(list=c("Boland.mask1","Boland.CH1"),file="./data/Boland.leopards1.rda")

model.a3.dW3 = list(D ~ s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) 
system.time(fit1.a3.dW3<-secrgam.fit(capthist=Boland.CH1,model=model.a3.dW3,mask=Boland.mask1,trace=FALSE))

model.a3 = list(D ~ s(alt,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1)
system.time(fit1.a3<-secrgam.fit(capthist=Boland.CH1,model=model.a3,mask=Boland.mask1,trace=FALSE))

model.a4 = list(D ~ s(alt,k=4,fx=TRUE), g0 ~ 1, sigma ~ 1)
system.time(fit1.a4<-secrgam.fit(capthist=Boland.CH1,model=model.a4,mask=Boland.mask1,trace=FALSE))

fit1.0<-secrgam.fit(capthist=Boland.CH1,model=list(D~1),mask=Boland.mask1,trace=FALSE)

AIC(fit1.a4,fit1.a3,fit1.a3.dW3,fit1.0)

plot(fit1.a3, asp = 1)
plot(fit1.a4, asp = 1)
points(cameras2, col="black",pch="+")
plot(fit1.a3.dW3, asp = 1)
points(cameras2, col="black",pch="+")
region.N(fit1.a4)
region.N(fit1.a3)
region.N(fit1.a3.dW3)
region.N(fit1.0)
plotDgam(fit1.a4)
A=nrow(Boland.mask1)*attr(Boland.mask1, "a")  # total area of mask
Dhat=Nhat/A              # density within mask

# Save data objects for package:
save(list=c("Boland.mask1","Boland.CH1"),file="./data/Boland.leopards1.rda")
save(list=c("Boland.mask2","Boland.CH2"),file="./data/Boland.leopards2.rda")

save(list=c("fit1.0","fit1.a3","fit1.a4","fit1.a3.dW3"),file="./data/Boland.fits1.rda")
save(list=c("fit2.0","fit2.a3.dW3","fit2.N.a3.dW3"),file="./data/Boland.fits2.rda")



# use a fitted model
popn = sim.popn.secrgam(fit=fit.ex,N=150)
head(popn)
dim(popn)
lCH=sim.capthist(cameras,popn,noccasions=13,detectpar=list(g0=0.19,sigma=3500),seed=123457)
plot(lCH,border=0,rad=500,tracks=TRUE,icolour=colors()[seq(2,202,10)],gridlines=FALSE)

model.N.a3.dW3 = list(D ~ Natural+s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) 
system.time(fit.N.a3.dW3<-secrgam.fit(capthist=lCH,model=model.N.a3.dW3,mask=Boland.mask,trace=FALSE))
model.a3.dW3 = list(D ~ s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) 
fit.a3.dW3<-secrgam.fit(capthist=lCH,model=model.a3.dW3,mask=Boland.mask,trace=FALSE)
fit.0<-secrgam.fit(capthist=lCH,model=list(D~1),mask=Boland.mask,trace=FALSE)

AIC(fit.N.a3.dW3,fit.0)

plot(fit.N.a3.dW3, asp = 1)
points(traps(Boland.CH), col="black",pch=19)
plotDgam(fit.N.a3.dW3)

Boland.CH1=lCH # dat with N=75 and seed=123457: Nice fit but bad (7.353) dAICc relative to fint.0


data(Boland.alt.image)
image.plot(Boland.alt.image,col=terrain.colors(20))
contour(Boland.alt.image,add=TRUE)
plot(cameras,add=TRUE)

data(Boland.landuse.image)
image.plot(Boland.landuse.image,col=terrain.colors(20))
contour(Boland.landuse.image,add=TRUE)
plot(cameras,add=TRUE)









# fit uniform model
fit0<-secrgam.fit(capthist=Boland.CH,model=list(D~1),mask=Boland.mask,trace=FALSE)
fit0 # look at fit results
# fit smooth of altitude
model=list(D~s(alt,k=4),g0~1,sigma~1) # smooth of altitude with 4 degrees of freedom
fit.a4<-secrgam.fit(capthist=Boland.CH,model=model,mask=Boland.mask,trace=FALSE)
fit.a4 # look at fit results
plot(fit.a4,asp=1)
plot(Boland.cameras,add=TRUE,detpar=list(pch="+",cex=1.2))
plotDgam(fit.a4,mask.rug=TRUE)
# compare the two models' AICs
AIC(fit.a4,fit0)
