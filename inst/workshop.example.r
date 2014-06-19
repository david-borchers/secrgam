library(secrgam)
data(Boland.leopards)

cameras=traps(Boland.CH)
image.plot.mask(Boland.mask,covariate="alt",asp=1,col=terrain.colors(30))
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask,covariate="dist2.Urban",asp=1)
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask,covariate="dist2.Water",asp=1)
plot(cameras,add=TRUE)
image.plot.mask(Boland.mask,covariate="LUfactor",asp=1)
plot(cameras,add=TRUE)

model.ex = list(D ~ Natural+s(alt,k=3,fx=TRUE)+s(dist2.Water,k=3,fx=TRUE), g0 ~ 1, sigma ~ 1) # smooth of altitude with 3 degrees of freedom
system.time(fit.ex <- secrgam.fit(capthist = Boland.CH, model = model.ex, mask = Boland.mask, trace = FALSE))
fit.ex <- secrgam.fit(capthist = Boland.CH, model = model.ex, mask = Boland.mask, trace = TRUE)
fit.ex # look at fit results
plot(fit.ex, asp = 1)
points(traps(Boland.CH), col="black",pch=19)
plotDgam(fit.ex)

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
