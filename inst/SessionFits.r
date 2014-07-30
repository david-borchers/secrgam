require(secrgam)
data(ovenbird)

AIC(ovenbird.model.1,ovenbird.model.1b,ovenbird.model.1T,ovenbird.model.D,ovenbird.model.h2)
ovenbird.model.1T
AIC(ovenbird.model.D)

par(mfrow = c(1,2))

# null model
fit0 = secrgam.fit(ovenCH, list(D = ~ 1), ovenmask, trace = FALSE)
print(AIC(fit0)[,"AIC"])

# Factor session
fit.session = secrgam.fit(ovenCH, list(D = ~ session), ovenmask, trace = FALSE)
print(AIC(fit0)[,"AIC"])

# smooth of Session df=2
ofit.S2 = secrgam.fit(ovenCH, list(D = ~ Session), ovenmask, trace = FALSE)
print(AIC(ofit.S2)[,"AIC"])
plotDgam(ofit.S2)

# smooth of Session df=3
ofit.S3 = secrgam.fit(ovenCH, list(D = ~ s(Session, k=3)), ovenmask, trace = FALSE)
print(AIC(ofit.S3)[,"AIC"])
plotDgam(ofit.S3)

AIC(fit0,ovenbird.model.D,fit.session,ofit.S3)
AIC(fit.session,ofit.S3,ovenbird.model.D)

# smooth of Session df=4
ofit.S4 = secrgam.fit(ovenCH, list(D = ~ s(Session, k=4)), ovenmask, trace = FALSE)
print(AIC(ofit.S4)[,"AIC"])
plotDgam(ofit.S4)

AIC(fit0,ovenbird.model.D,fit.session,ofit.S3,ofit.S4)
AIC(fit.session,ofit.S3,ofit.S4,ovenbird.model.D)

# smooth of Session df=5
ofit.S5 = secrgam.fit(ovenCH, list(D = ~ s(Session, k=5)), ovenmask, trace = FALSE)
print(AIC(ofit.S5)[,"AIC"])
plotDgam(ofit.S5)

AIC(fit0,ovenbird.model.D,fit.session,ofit.S3,ofit.S4,ofit.S5)
AIC(fit.session,ofit.S3,ofit.S4,ofit.S5,ovenbird.model.D)

# should be exactly the same as above (just to show you can use your own covariates)
fit2 = secrgam.fit(ovenCH, list(D = ~ s(scov, k=3)), ovenmask, sessioncov = data.frame(scov = 0:4), trace = FALSE)
print(AIC(fit2)[,"AIC"])
plotDgam(fit2)