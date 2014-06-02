
data(Boland.leopards)

# model = list(D ~ s(alt, k = 3), sigma ~ 1)

model = list(D ~ te(x, y, k = 3), sigma ~ 1)

Boland.fit = secrgam.fit(capthist = Boland.CH, model = model, fixed = list(g0 = 0.24), mask = Boland.mask, trace = FALSE)

print(Boland.fit)

save(Boland.mask, Boland.CH, Boland.image, Boland.fit, file = "~/Packages/secrgam/data/Boland.leopards.rda")
