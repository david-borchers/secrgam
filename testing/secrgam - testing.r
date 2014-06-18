## ************************************************************************** ##
## ************************************************************************** ##
##                                                                            ##
##                            secrgam - testing                               ##
##                                                                            ##
## ************************************************************************** ##
## ************************************************************************** ##

require(secrgam)

data(Boland.leopards)

# mask and spatially referereced covariate (alt)
head(Boland.mask)
head(attr(Boland.mask, "covariates"))

# image plot
image(Boland.image, col = terrain.colors(60), asp = 1)
contour(Boland.image, add = TRUE)
plot(traps(Boland.CH), add = TRUE, detpar = list(pch = "+", cex = 1.2))

# summarise and plot capture histories
summary(Boland.CH)
plot(Boland.CH, border = 0, rad = 0.01, tracks = TRUE, icolour = colors()[seq(2, 202, 10)])

# fixed density
fit0 = secrgam.fit(
  capthist = Boland.CH, 
  model    = list(g0 ~ 1, sigma ~ 1), 
  fixed    = list(D = 0.0001535589),
  mask     = Boland.mask, 
  trace    = FALSE
)
predict(fit0)
head(fitted(fit0))
plot(fit0, asp = 1)

# null model - uniform density
fit0 = secrgam.fit(
  capthist = Boland.CH, 
  model    = list(D ~ 1, g0 ~ 1, sigma ~ 1), 
  mask     = Boland.mask, 
  trace    = FALSE
)
predict(fit0)
head(fitted(fit0))
plot(fit0, asp = 1)
AIC(fit0)[,"AIC"] # 693.406


# 1D smooth for altitude
fit1 = secrgam.fit(
  capthist = Boland.CH, 
  model    = list(D ~ s(alt, k = 3), g0 ~ 1, sigma ~ 1), 
  mask     = Boland.mask, 
  trace    = FALSE
)
predict(fit1)
head(fitted(fit1))
plot(fit1, asp = 1)
AIC(fit1)[,"AIC"] # 696.44


# 2D smooth for x, y
fit2 = secrgam.fit(
  capthist = Boland.CH, 
  model    = list(D ~ te(x, y, k = 3), g0 ~ 1, sigma ~ 1), 
  mask     = Boland.mask, 
  trace    = FALSE
)
predict(fit2)
head(fitted(fit2))
plot(fit2, asp = 1)
AIC(fit2)[,"AIC"] # 703.472


# 3D smooth for x, y and altitude
fit3 = secrgam.fit(
  capthist = Boland.CH, 
  model    = list(D ~ te(x, y, alt, k = 3), g0 ~ 1, sigma ~ 1), 
  mask     = Boland.mask, 
  trace    = FALSE
)
predict(fit3)
head(fitted(fit3))
plot(fit3, asp = 1)
AIC(fit3)[,"AIC"] # 728.433


# plots -------------------------------------------------------------------

fit = fit1

jpeg("test.plots.secrgam.jpeg", height = 20, width = 10, units = "cm", pointsize = 8, quality = 150, res = 300, family = "serif")

par(mfrow = c(2,1), cex = 1, cex.main = 1)

# fitted density
X = cbind(fit1$mask, attributes(fit$mask)$covariates)
D = exp(as.numeric(model.matrix(fit$model$D, X) %*% fit$fit$par[fit$parindx$D])) * 100

prep4image(data.frame(Boland.mask[,c("x","y")], z = D), asp = 1, col = heat.colors(10), main = "fitted density - D ~ s(alt, k = 3)")
points(as.data.frame(traps(Boland.CH)), col = 4, pch = 15)

# fitted fxi
fxi = fxi.secrgam(fit) ; sum(fxi)
prep4image(data.frame(Boland.mask[,c("x","y")], z = fxi), asp = 1, col = heat.colors(10), main = "fitted fxi for animal 1")
points(as.data.frame(traps(Boland.CH)), col = 4, pch = 15)

dev.off()



# require(lattice)
# levelplot(fxi ~ x * y, data = Boland.mask, asp = 1, contour = TRUE, labels = TRUE, pretty = TRUE, zlim = c(0,1))
# points(as.data.frame(traps(Boland.CH)), col = 4, pch = 15)

# estimates
# predict(fit)["D","estimate"]
# predict(fit)["g0","estimate"]
# predict(fit)["sigma","estimate"]


# plot fitted surface:
# Dsurface = predictDsurface(fit)
# plot(Dsurface, border = 0, polyc = 'blue', plottype = 'shaded', col = terrain.colors(30), breaks = 25, meshcol = NA)
# plot(traps(Boland.CH), add = TRUE)



# plot smooths:
plotDgam(fit)












## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
## ************************************************************************** ##
