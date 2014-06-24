library(secrgam)
data(Boland.leopards)
load("Boland.mask.rda") # revised mask to include landuse and related covariates

cameras=traps(Boland.CH)
image.plot.mask(Boland.mask,covariate="alt",asp=1,col=terrain.colors(30))
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask,covariate="dist2.Urban",asp=1)
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask,covariate="dist2.Water",asp=1)
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask,covariate="LUfactor",asp=1)
plot(cameras,add=TRUE)

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

#save(list=c(Boland.mask2,cameras2),file="./data/Boland.leopards2.rda")

image.plot.mask(Boland.mask2,covariate="alt",asp=1); points(cameras2,pch="+")
image.plot.mask(Boland.mask2,covariate="dist2.Urban",asp=1); points(cameras2,pch="+")
image.plot.mask(Boland.mask2,covariate="dist2.Water",asp=1); points(cameras2,pch="+")


# use a fitted model
popn = sim.popn.secrgam(fit=fit.ex,N=150,mask=Boland.mask2)
head(popn)
dim(popn)
simCH=sim.capthist(cameras2,popn,noccasions=13,detectpar=list(g0=0.19,sigma=2500),seed=123457)
plot(simCH,border=0,rad=500,tracks=TRUE,icolour=colors()[seq(2,202,10)],gridlines=FALSE)

model.N.a3.dW3 = list(D ~ Natural+s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) 
system.time(fit2.N.a3.dW3<-secrgam.fit(capthist=simCH,model=model.N.a3.dW3,mask=Boland.mask2,trace=FALSE))

model.a3.dW3 = list(D ~ s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) 
system.time(fit2.a3.dW3<-secrgam.fit(capthist=simCH,model=model.a3.dW3,mask=Boland.mask2,trace=FALSE))

fit2.0<-secrgam.fit(capthist=simCH,model=list(D~1),mask=Boland.mask2,trace=FALSE)

AIC(fit2.N.a3.dW3,fit2.a3.dW3,fit2.0)

plot(fit2.a3.dW3, asp = 1)
points(cameras2, col="black",pch="+")
region.N(fit2.a3.dW3)
region.N(fit2.0)
plotDgam(fit2.N.a3.dW3)








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
