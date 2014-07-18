require(secrgam)
data(ovenbird)

AIC(ovenbird.model.1,ovenbird.model.1b,ovenbird.model.1T,ovenbird.model.D,ovenbird.model.h2)
ovenbird.model.1T
AIC(ovenbird.model.D)

par(mfrow = c(1,2))

# null model
fit0 = secrgam.fit(ovenCH, list(D = ~ 1), ovenmask, trace = FALSE)
print(AIC(fit0)[,"AIC"])

# log-linear session
fit.session = secrgam.fit(ovenCH, list(D = ~ session), ovenmask, trace = FALSE)
print(AIC(fit0)[,"AIC"])

# smooth of Session
fit1 = secrgam.fit(ovenCH, list(D = ~ s(Session, k=3)), ovenmask, trace = FALSE)
print(AIC(fit1)[,"AIC"])
plotDgam(fit1)

AIC(fit0,ovenbird.model.D,fit.session,fit1)
AIC(fit.session,fit1,ovenbird.model.D)

# should be exactly the same as above (just to show you can use your own covariates)
fit2 = secrgam.fit(ovenCH, list(D = ~ s(scov, k=3)), ovenmask, sessioncov = data.frame(scov = 0:4), trace = FALSE)
print(AIC(fit2)[,"AIC"])
plotDgam(fit2)