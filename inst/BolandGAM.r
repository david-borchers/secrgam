chist=read.table("Unique Capture data file with heading.txt",header=TRUE)
cameras=read.traps("Detector layout file.txt",detector="proximity")
cameras$y=-cameras$y
# Get Altitude data in right format
gis=read.table("./Elevation data/WCape.xyz")
names(gis)=c("x","y","z")
WCape.alt=gis
WCape.alt$z[WCape.alt$z<=0]=NA


ht=prep4image(WCape.alt,plot=TRUE,col=terrain.colors(30))
contour(alt,add=TRUE)
plot(cameras,add=TRUE,detpar=list(pch="+",cex=1.2))
# make mask and capthist objects with altitude data in attributes(mask)$covariates
tempmask=make.mask(cameras,buffer=0.12,spacing=0.01,type='trapbuffer')
hts=data.frame(alt=interp.surface(ht, tempmask))
maskdat=data.frame(x=tempmask$x,y=tempmask$y,alt=hts$alt,
                   xalt=tempmask$x*hts$alt/sum(tempmask$x*hts$alt),
                   yalt=tempmask$y*hts$alt/sum(tempmask$y*hts$alt))
mask=read.mask(data=maskdat)


capthist=make.capthist(chist,cameras)
# do zoomed-in plot:
xlim=c(18.85,19.55)
ylim=c(-34.05,-33.4)
image(ht,xlim=xlim,ylim=ylim,col=terrain.colors(60))
contour(ht,xlim=xlim,ylim=ylim,add=TRUE)
plot(cameras,add=TRUE,detpar=list(pch="+",cex=1.2))
points(mask,pch=".")

model=list(D~1,g0~1,sigma~1)
model=list(D~s(alt,k=2),g0~1,sigma~1)
# fit secr GAM model
system.time(fit<-secr.fit.gam(capthist=capthist,model=model,mask=mask,trace=FALSE))
fit # look at fit
# plot fitted surface:
Dsurface <- predictDsurface(fit)
plot(Dsurface,border=0.0, polyc='blue', plottype='shaded',col=terrain.colors(30), breaks=25, meshcol=NA)
plot(traps(capthist), add = TRUE)
# plot smooths:
plotDgam(fit)

plot(Dsurface,border=0.0, polyc='blue', plottype='shaded',col=terrain.colors(30), breaks=25, meshcol=NA)
plot(traps(capthist), add = TRUE)
pdot.contour(cameras,noccasions=13,detectpar=list(g0=1.897923e-01,sigma=3.225914e-02),nx=100,ny=100,col="blue",add=TRUE)

